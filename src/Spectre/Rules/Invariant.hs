{-# LANGUAGE OverloadedStrings #-}
-- | Invariant-related inspection rules.
--
-- Checks for missing input validation, unsafe partial functions,
-- and floating-point equality comparisons.
module Spectre.Rules.Invariant
  ( invariantRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Spectre.Ast
import Spectre.Inspection
import Spectre.Rules.Utils (isNumericField, isTimeField)

-- | All invariant rules
invariantRules :: [Inspection]
invariantRules =
  [ missingInputValidation
  , unsafePartialFunctions
  , floatingPointEquality
  , asymmetricValidation
  , unusedFetchResult
  , failOpenOptional
  , selfAssignmentWithoutGuard
  , missingUpperBoundValidation
  , asymmetricSiblingValidation
  , missingCrossEntityValidation
  , timeFieldNotInEnsure
  , indirectParamAssignmentNoGuard
  , uncanonicalisedListPersist
  , unsafeDivision
  ]

-- | SPEC-INV-001: Missing input validation on choice parameters
--
-- Detects choices where user-supplied parameters are used in create
-- statements without prior validation (assertMsg).
missingInputValidation :: Inspection
missingInputValidation = mkInspection
  "SPEC-INV-001"
  "Missing input validation"
  "Choice parameter is used in a create statement without prior assertion/validation. User-controlled input should be validated before use."
  Warning
  [Invariant]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INV-001") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': parameter '" <> fieldName param
         <> "' is used in a create statement without prior validation")
        (Just $ "Add 'assertMsg \"invalid " <> fieldName param <> "\" (<validation>)' before the create")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , param <- chParams ch
    , paramUsedInCreate (fieldName param) (chBody ch)
    , not (paramValidatedBefore (fieldName param) (chBody ch))
    ]

-- | SPEC-INV-002: Unsafe partial functions
--
-- Detects use of partial functions (head, tail, !!) that will crash
-- on empty lists. These should be guarded with null checks or use
-- safe alternatives.
unsafePartialFunctions :: Inspection
unsafePartialFunctions = mkInspection
  "SPEC-INV-002"
  "Unsafe partial function"
  "Use of partial function (head, tail, !!) that will crash on empty input. Use safe alternatives or add a guard."
  Warning
  [Invariant]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INV-002") Warning (loc)
        ("Template '" <> tplName tpl <> "', choice '" <> chName ch
         <> "': use of partial function '" <> funcName <> "' without guard")
        (Just $ "Add a 'not (null xs)' guard before using '" <> funcName <> "', or use a safe alternative")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , (funcName, loc) <- findPartialFunctions (chBody ch)
    , not (hasNullGuard funcName (chBody ch))
    ]

-- | SPEC-INV-003: Floating-point equality comparison
--
-- Detects use of == on Decimal/numeric expressions, which can give
-- unexpected results due to floating-point precision issues.
floatingPointEquality :: Inspection
floatingPointEquality = mkInspection
  "SPEC-INV-003"
  "Floating-point equality comparison"
  "Equality comparison (==) on numeric expressions that may involve floating-point arithmetic. Consider using an epsilon-based comparison."
  Warning
  [Invariant]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INV-003") Warning loc
        ("Template '" <> tplName tpl <> "', choice '" <> chName ch
         <> "': equality comparison (==) on potentially computed numeric value")
        (Just "Consider using 'abs(a - b) < epsilon' for computed values")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , loc <- findDecimalEquality (chBody ch)
    ]

-- Helpers

partialFunctions :: [Text]
partialFunctions = ["head", "tail", "!!", "fromSome", "fromJust", "init", "last"]

-- | Check if a parameter name is used in any create statement
paramUsedInCreate :: Text -> [Stmt] -> Bool
paramUsedInCreate name stmts = any check stmts
  where
    check (SCreate _ expr _) = exprMentionsVar name expr
    check (SBind _ expr _) = exprHasCreateWith name expr
    check (SExpr expr _) = exprHasCreateWith name expr
    check _ = False

    exprHasCreateWith n (EApp (EVar "create" _) arg _) = exprMentionsVar n arg
    exprHasCreateWith n (EApp f a _) = exprHasCreateWith n f || exprHasCreateWith n a
    exprHasCreateWith _ _ = False

-- | Check if a parameter is validated (assertMsg) before its first use in a create
paramValidatedBefore :: Text -> [Stmt] -> Bool
paramValidatedBefore name stmts =
  let -- Only check statements BEFORE the first create that uses this param
      stmtsBefore = takeWhile (not . isCreateUsing name) stmts
  in any check stmtsBefore
  where
    check (SAssert _ cond _) = exprMentionsVar name cond
    check (SExpr e _) = isAssertOn name e
    check _ = False

    isAssertOn n (EApp (EApp (EVar "assertMsg" _) _ _) cond _) = exprMentionsVar n cond
    isAssertOn _ _ = False

    isCreateUsing n (SCreate _ expr _) = exprMentionsVar n expr
    isCreateUsing n (SBind _ expr _) = exprHasCreateWith n expr
    isCreateUsing n (SExpr expr _) = exprHasCreateWith n expr
    isCreateUsing _ _ = False

    exprHasCreateWith n (EApp (EVar "create" _) arg _) = exprMentionsVar n arg
    exprHasCreateWith n (EApp f a _) = exprHasCreateWith n f || exprHasCreateWith n a
    exprHasCreateWith _ _ = False

-- | Check if an expression mentions a variable name
exprMentionsVar :: Text -> Expr -> Bool
exprMentionsVar name (EVar v _) = v == name
exprMentionsVar name (EApp f a _) = exprMentionsVar name f || exprMentionsVar name a
exprMentionsVar name (EInfix _ l r _) = exprMentionsVar name l || exprMentionsVar name r
exprMentionsVar name (EFieldAccess e _ _) = exprMentionsVar name e
exprMentionsVar name (EParens e _) = exprMentionsVar name e
exprMentionsVar name (EIf c t e _) = exprMentionsVar name c || exprMentionsVar name t || exprMentionsVar name e
exprMentionsVar name (ELet binds body _) = any (exprMentionsVar name . bindExpr) binds || exprMentionsVar name body
exprMentionsVar name (ETuple es _) = any (exprMentionsVar name) es
exprMentionsVar name (EList es _) = any (exprMentionsVar name) es
exprMentionsVar name (ERecordCon _ fields _) = any (exprMentionsVar name . snd) fields
exprMentionsVar name (ERecordUpd e fields _) = exprMentionsVar name e || any (exprMentionsVar name . snd) fields
exprMentionsVar name (ECase e alts _) = exprMentionsVar name e || any (exprMentionsVar name . snd) alts
exprMentionsVar name (EDo stmts _) = any (stmtMentionsVar name) stmts
exprMentionsVar _ _ = False

-- | Check if a statement mentions a variable
stmtMentionsVar :: Text -> Stmt -> Bool
stmtMentionsVar name (SExpr e _) = exprMentionsVar name e
stmtMentionsVar name (SBind _ e _) = exprMentionsVar name e
stmtMentionsVar name (SCreate _ e _) = exprMentionsVar name e
stmtMentionsVar name (SAssert _ e _) = exprMentionsVar name e
stmtMentionsVar name (SLet binds _) = any (exprMentionsVar name . bindExpr) binds
stmtMentionsVar name (SReturn e _) = exprMentionsVar name e
stmtMentionsVar _ _ = False

-- | Find uses of partial functions in statements
findPartialFunctions :: [Stmt] -> [(Text, SrcSpan)]
findPartialFunctions = concatMap findInStmt
  where
    findInStmt (SExpr e _) = findInExpr e
    findInStmt (SBind _ e _) = findInExpr e
    findInStmt (SAssert _ e _) = findInExpr e
    findInStmt (SLet binds _) = concatMap (findInExpr . bindExpr) binds
    findInStmt (SCreate _ e _) = findInExpr e
    findInStmt (SReturn e _) = findInExpr e
    findInStmt _ = []

    findInExpr (EVar name s)
      | name `elem` partialFunctions = [(name, s)]
    findInExpr (EApp f a _) = findInExpr f ++ findInExpr a
    findInExpr (EInfix _ l r _) = findInExpr l ++ findInExpr r
    findInExpr (EParens e _) = findInExpr e
    findInExpr _ = []

-- | Check if there's a null guard for a specific partial function usage.
-- The funcName is the name of the partial function (e.g., "head", "tail").
-- We look for null checks on any list variable that the partial function
-- is applied to in the same scope.
hasNullGuard :: Text -> [Stmt] -> Bool
hasNullGuard funcName stmts =
  let -- Find list variables that funcName is applied to
      targetVars = concatMap (findPartialArgVars funcName) stmts
  in any (\v -> any (isNullCheckOn v) stmts) targetVars
  where
    isNullCheckOn v (SAssert _ cond _) = exprHasNullCheckOn v cond
    isNullCheckOn v (SExpr (EIf cond _ _ _) _) = exprHasNullCheckOn v cond
    isNullCheckOn _ _ = False

    exprHasNullCheckOn v (EApp (EVar "not" _) (EApp (EVar "null" _) (EVar x _) _) _) = x == v
    exprHasNullCheckOn v (EInfix "/=" (EApp (EVar "length" _) (EVar x _) _) (ENum "0" _) _) = x == v
    exprHasNullCheckOn v (EApp (EVar "null" _) (EVar x _) _) = x == v
    exprHasNullCheckOn _ _ = False

-- | Find variables that a partial function is applied to
findPartialArgVars :: Text -> Stmt -> [Text]
findPartialArgVars fn (SExpr e _) = findPartialArgInExpr fn e
findPartialArgVars fn (SBind _ e _) = findPartialArgInExpr fn e
findPartialArgVars fn (SAssert _ e _) = findPartialArgInExpr fn e
findPartialArgVars fn (SLet binds _) = concatMap (findPartialArgInExpr fn . bindExpr) binds
findPartialArgVars fn (SCreate _ e _) = findPartialArgInExpr fn e
findPartialArgVars fn (SReturn e _) = findPartialArgInExpr fn e
findPartialArgVars _ _ = []

findPartialArgInExpr :: Text -> Expr -> [Text]
findPartialArgInExpr fn (EApp (EVar f _) (EVar v _) _) | f == fn = [v]
findPartialArgInExpr fn (EApp f a _) = findPartialArgInExpr fn f ++ findPartialArgInExpr fn a
findPartialArgInExpr fn (EInfix _ l r _) = findPartialArgInExpr fn l ++ findPartialArgInExpr fn r
findPartialArgInExpr fn (EParens e _) = findPartialArgInExpr fn e
findPartialArgInExpr _ _ = []

-- | Find equality comparisons on computed numeric values.
-- Two-pass: first collect let-bound variables that alias computed numerics,
-- then find == comparisons involving those variables or direct computations.
findDecimalEquality :: [Stmt] -> [SrcSpan]
findDecimalEquality stmts =
    let computedVars = collectComputedVars stmts
    in concatMap (findInStmt computedVars) stmts
  where
    -- First pass: collect variable names bound to computed numeric expressions
    collectComputedVars :: [Stmt] -> [Text]
    collectComputedVars = concatMap cvStmt
      where
        cvStmt (SLet binds _) =
          [ bindName b | b <- binds, isComputedNumeric (bindExpr b) ]
        cvStmt _ = []

    findInStmt cv (SExpr e _) = findInExpr cv e
    findInStmt cv (SAssert _ e _) = findInExpr cv e
    findInStmt cv (SBind _ e _) = findInExpr cv e
    findInStmt cv (SLet binds _) = concatMap (findInExpr cv . bindExpr) binds
    findInStmt cv (SReturn e _) = findInExpr cv e
    findInStmt _ _ = []

    findInExpr cv (EInfix "==" l r s)
      | isComputedOrAlias cv l || isComputedOrAlias cv r = [s]
    findInExpr cv (EApp f a _) = findInExpr cv f ++ findInExpr cv a
    findInExpr cv (EParens e _) = findInExpr cv e
    findInExpr cv (EIf c t e _) = findInExpr cv c ++ findInExpr cv t ++ findInExpr cv e
    findInExpr cv (ECase e alts _) = findInExpr cv e ++ concatMap (findInExpr cv . snd) alts
    findInExpr cv (EDo innerStmts _) = concatMap (findInStmt cv) innerStmts
    findInExpr _ _ = []

    -- A numeric expression is "computed" if it involves arithmetic,
    -- or is a variable known to be bound to a computed numeric
    isComputedOrAlias :: [Text] -> Expr -> Bool
    isComputedOrAlias cv (EVar v _) = v `elem` cv
    isComputedOrAlias cv (EParens e _) = isComputedOrAlias cv e
    isComputedOrAlias _ e = isComputedNumeric e

    -- A numeric expression is directly "computed" if it involves arithmetic
    isComputedNumeric (EInfix op _ _ _)
      | op `elem` ["+", "-", "*", "/"] = True
    isComputedNumeric (EApp (EVar f _) _ _)
      | f `elem` ["round", "truncate", "floor", "ceiling", "fromRational", "intToDecimal"] = True
    isComputedNumeric _ = False

-- | SPEC-INV-004: Asymmetric validation across sibling choices
--
-- Detects templates where some choices validate template fields with
-- assertMsg but other sibling choices in the same template do not,
-- creating inconsistent invariant enforcement.
asymmetricValidation :: Inspection
asymmetricValidation = mkInspection
  "SPEC-INV-004"
  "Asymmetric validation across choices"
  "Some choices validate template fields while sibling choices do not, creating inconsistent invariant enforcement."
  Warning
  [Invariant]
  checkAsymmetricValidation

checkAsymmetricValidation :: Module -> [Finding]
checkAsymmetricValidation mod_ =
  let templates = [tpl | DTemplate tpl <- moduleDecls mod_]
  in concatMap checkTemplate templates
  where
    checkTemplate tpl
      | length choices < 2 = []
      | null tplFieldNames = []
      | otherwise =
          [ mkFinding (InspectionId "SPEC-INV-004") Warning (chLocation noAssertCh)
              ("Template '" <> tplName tpl <> "', choice '" <> chName noAssertCh
               <> "' has no validation assertions, but sibling choice '"
               <> chName assertCh <> "' validates template field '"
               <> validatedField <> "'")
              (Just $ "Add matching assertions to choice '" <> chName noAssertCh <> "' for consistent invariant enforcement")
              (Just (tplName tpl))
              (Just (chName noAssertCh))
          | assertCh <- choices
          , let validated = choiceValidatesFields tplFieldNames (chBody assertCh)
          , not (null validated)
          , let validatedField = head validated
          , noAssertCh <- choices
          , chName noAssertCh /= chName assertCh
          , null (choiceValidatesFields tplFieldNames (chBody noAssertCh))
          ]
      where
        choices = tplChoices tpl
        tplFieldNames = map fieldName (tplFields tpl)

-- | Find template field names validated in assertMsg within a choice body
choiceValidatesFields :: [Text] -> [Stmt] -> [Text]
choiceValidatesFields tplFields stmts =
  [ f | f <- tplFields, any (stmtAssertsMentions f) stmts ]
  where
    stmtAssertsMentions f (SAssert _ cond _) = exprMentionsVar f cond
    stmtAssertsMentions f (SExpr e _) = exprIsAssertOn f e
    stmtAssertsMentions _ _ = False

    exprIsAssertOn f (EApp (EApp (EVar "assertMsg" _) _ _) cond _) = exprMentionsVar f cond
    exprIsAssertOn f (EApp func _ _) = exprIsAssertOn f func
    exprIsAssertOn _ _ = False

-- | SPEC-INV-005: Unused fetch result
--
-- Detects when a fetch result is discarded (bound to _ or underscore-prefixed
-- variable). Fetching a contract but ignoring the result often means the
-- choice is not validating fetched data against its own parameters.
unusedFetchResult :: Inspection
unusedFetchResult = mkInspection
  "SPEC-INV-005"
  "Unused fetch result"
  "A fetch result is discarded or bound to an unused variable. The fetched contract data should be validated against choice parameters."
  Warning
  [Invariant]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INV-005") Warning loc
        ("Template '" <> tplName tpl <> "', choice '" <> chName ch
         <> "': fetch result is discarded (bound to '" <> varName
         <> "') — fetched data is not validated")
        (Just "Use the fetched contract's fields to validate choice parameters or compute values")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , (varName, loc) <- findUnusedFetches (chBody ch)
    ]

-- | Find fetch statements whose result is bound to an underscore-prefixed variable
-- or a wildcard pattern.
findUnusedFetches :: [Stmt] -> [(Text, SrcSpan)]
findUnusedFetches stmts = concatMap checkStmt stmts
  where
    checkStmt (SBind pat expr s)
      | isFetchExpr expr, isUnusedPat pat = [(patName pat, s)]
    checkStmt _ = []

    isFetchExpr (EApp (EVar "fetch" _) _ _) = True
    isFetchExpr _ = False

    isUnusedPat (PWild _) = True
    isUnusedPat (PVar name _) = "_" `T.isPrefixOf` name
    isUnusedPat _ = False

    patName (PWild _) = "_"
    patName (PVar name _) = name
    patName _ = "_"

-- | SPEC-INV-006: Fail-open Optional pattern
--
-- Detects case expressions on Optional values where the None branch
-- silently succeeds (pure () / return ()) instead of failing.  This
-- means a missing configuration is silently ignored.
-- Works on BOTH template choices and top-level functions.
failOpenOptional :: Inspection
failOpenOptional = mkInspection
  "SPEC-INV-006"
  "Fail-open Optional pattern"
  "A case expression on an Optional value silently succeeds in the None branch (pure ()), meaning the absence of configuration is ignored rather than rejected."
  Warning
  [Invariant]
  $ \mod_ ->
    -- (1) In template choices
    [ mkFinding (InspectionId "SPEC-INV-006") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': None branch silently succeeds — missing value is ignored instead of rejected")
        (Just "Replace 'None -> pure ()' with 'None -> assertMsg \"must be configured\" False' or abort")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , hasFailOpenOptional (chBody ch)
    ]
    ++
    -- (2) In top-level functions
    [ mkFinding (InspectionId "SPEC-INV-006") Warning loc
        ("Function '" <> name
         <> "': None branch silently succeeds — missing value is ignored instead of rejected")
        (Just "Replace 'None -> pure ()' with 'None -> assertMsg \"must be configured\" False' or abort")
        Nothing
        Nothing
    | DFunction name _ body loc <- moduleDecls mod_
    , exprHasFailOpenOptional body
    ]

-- | Check if a list of statements contains a fail-open Optional pattern
hasFailOpenOptional :: [Stmt] -> Bool
hasFailOpenOptional = any stmtHasFOO
  where
    stmtHasFOO (SExpr e _) = exprHasFailOpenOptional e
    stmtHasFOO (SBind _ e _) = exprHasFailOpenOptional e
    stmtHasFOO (SReturn e _) = exprHasFailOpenOptional e
    stmtHasFOO (SLet binds _) = any (exprHasFailOpenOptional . bindExpr) binds
    stmtHasFOO _ = False

exprHasFailOpenOptional :: Expr -> Bool
exprHasFailOpenOptional (ECase _ alts _) =
  any isNoneBranchSilentSuccess alts
exprHasFailOpenOptional (EApp f a _) =
  exprHasFailOpenOptional f || exprHasFailOpenOptional a
exprHasFailOpenOptional (EParens e _) = exprHasFailOpenOptional e
exprHasFailOpenOptional (EIf _ t e _) =
  exprHasFailOpenOptional t || exprHasFailOpenOptional e
exprHasFailOpenOptional (EDo stmts _) = hasFailOpenOptional stmts
exprHasFailOpenOptional _ = False

-- | Check if a case alternative is a None -> pure () / return () pattern
isNoneBranchSilentSuccess :: (Pattern, Expr) -> Bool
isNoneBranchSilentSuccess (pat, body) =
  isNonePattern pat && isSilentSuccess body

isNonePattern :: Pattern -> Bool
isNonePattern (PVar "None" _) = True
isNonePattern (PCon "None" [] _) = True
isNonePattern _ = False

isSilentSuccess :: Expr -> Bool
-- pure ()
isSilentSuccess (EApp (EVar "pure" _) (ETuple [] _) _) = True
-- return ()
isSilentSuccess (EApp (EVar "return" _) (ETuple [] _) _) = True
-- pure () in parens
isSilentSuccess (EParens e _) = isSilentSuccess e
-- do { pure () } / do { return () }
isSilentSuccess (EDo [SReturn (ETuple [] _) _] _) = True
isSilentSuccess (EDo [SExpr e _] _) = isSilentSuccess e
isSilentSuccess _ = False

-- | SPEC-INV-007: Self-assignment without change guard
--
-- Detects choices that accept a parameter and use it in
-- `create this with { field = param }` without first asserting
-- `param /= field` (or equivalent).  This allows a no-op state
-- transition that still emits events and burns gas.
-- Works on both template choices and top-level functions with
-- record-update patterns.
selfAssignmentWithoutGuard :: Inspection
selfAssignmentWithoutGuard = mkInspection
  "SPEC-INV-007"
  "Self-assignment without change guard"
  "A choice parameter is assigned directly to a template field via 'create this with { field = param }' without checking that the new value differs from the old."
  Warning
  [Invariant, StateIntegrity]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INV-007") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': parameter '" <> paramN
         <> "' is assigned to field '" <> fieldN
         <> "' without asserting it differs from the current value")
        (Just $ "Add 'assertMsg \"no change\" (" <> paramN <> " /= " <> fieldN <> ")' before the create")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , (paramN, fieldN) <- findSelfAssignments ch tpl
    , not (hasChangeGuard paramN fieldN (chBody ch))
    ]

-- | Find (paramName, fieldName) pairs where a choice parameter is directly
-- assigned to a template field in a record update (create this with { ... })
findSelfAssignments :: Choice -> Template -> [(Text, Text)]
findSelfAssignments ch tpl =
  let paramNames = map fieldName (chParams ch)
      tplFieldNames = map fieldName (tplFields tpl)
  in [ (pName, fName)
     | (fName, assignedExpr) <- concatMap recordUpdateFields (chBody ch)
     , fName `elem` tplFieldNames
     , pName <- extractVarName assignedExpr
     , pName `elem` paramNames
     ]

-- | Extract field assignments from record update statements
recordUpdateFields :: Stmt -> [(Text, Expr)]
recordUpdateFields (SCreate _ (ERecordUpd _ fields _) _) = fields
recordUpdateFields (SBind _ (EApp (EVar "create" _) (ERecordUpd _ fields _) _) _) = fields
recordUpdateFields (SExpr (EApp (EVar "create" _) (ERecordUpd _ fields _) _) _) = fields
recordUpdateFields _ = []

extractVarName :: Expr -> [Text]
extractVarName (EVar name _) = [name]
extractVarName (EParens e _) = extractVarName e
extractVarName _ = []

-- | Check if the choice body has a guard asserting param /= field
hasChangeGuard :: Text -> Text -> [Stmt] -> Bool
hasChangeGuard paramN fieldN stmts = any check stmts
  where
    check (SAssert _ cond _) = exprHasNeqCheck paramN fieldN cond
    check (SExpr e _) = exprIsAssertNeq paramN fieldN e
    check _ = False

    exprHasNeqCheck p f (EInfix "/=" l r _) = mentionsBoth p f l r
    exprHasNeqCheck p f (EApp (EVar "not" _) (EInfix "==" l r _) _) = mentionsBoth p f l r
    exprHasNeqCheck _ _ _ = False

    exprIsAssertNeq p f (EApp (EApp (EVar "assertMsg" _) _ _) cond _) = exprHasNeqCheck p f cond
    exprIsAssertNeq _ _ _ = False

    mentionsBoth p f l r =
      (exprMentionsVar p l && exprMentionsVar f r)
      || (exprMentionsVar f l && exprMentionsVar p r)

-- | SPEC-INV-008: Missing upper-bound validation
--
-- Detects choices where a numeric parameter is validated with a lower
-- bound (>= 0) but not an upper bound, allowing extreme values that
-- would break business logic (e.g., a fee of 100%).
missingUpperBoundValidation :: Inspection
missingUpperBoundValidation = mkInspection
  "SPEC-INV-008"
  "Missing upper-bound validation"
  "A numeric parameter is validated with a lower bound check but no upper bound check. Extreme values may break business logic."
  Warning
  [Invariant]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INV-008") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': parameter '" <> paramN
         <> "' has lower-bound check but no upper-bound check — extreme values are allowed")
        (Just $ "Add an upper-bound assertion, e.g., 'assertMsg \"too large\" (" <> paramN <> " < maxValue)'")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , paramN <- findParamsWithOnlyLowerBound ch
    ]
    ++
    -- Also check ensure clauses: lower bound present but no upper bound
    [ mkFinding (InspectionId "SPEC-INV-008") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl
         <> "': field '" <> fieldN
         <> "' has lower-bound ensure check but no upper-bound ensure check — extreme values are allowed")
        (Just $ "Add an upper-bound to ensure, e.g., 'ensure " <> fieldN <> " >= 0 && " <> fieldN <> " < maxValue'")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , Just ensureExpr <- [tplEnsure tpl]
    , fieldN <- findFieldsWithOnlyLowerBoundInEnsure (tplFields tpl) ensureExpr
    ]

-- | Find choice parameters that have a lower-bound assert (>= 0) but no upper-bound
findParamsWithOnlyLowerBound :: Choice -> [Text]
findParamsWithOnlyLowerBound ch =
  let paramNames = map fieldName (chParams ch)
      stmts = chBody ch
  in [ p | p <- paramNames
     , paramHasLowerBound p stmts
     , not (paramHasUpperBound p stmts)
     , isNumericParam p ch
     ]

isNumericParam :: Text -> Choice -> Bool
isNumericParam name ch =
  case [fieldType f | f <- chParams ch, fieldName f == name] of
    (Just (TCon ty _):_) -> ty `elem` ["Int", "Decimal", "Numeric", "Integer"]
    _ -> nameHintIsNumeric name
  where
    nameHintIsNumeric n =
      let lower = T.toLower n
      in any (`T.isInfixOf` lower) ["amount", "rate", "fee", "bps", "percent", "count", "num", "qty", "quantity", "price", "limit", "balance", "total", "margin", "delta"]

paramHasLowerBound :: Text -> [Stmt] -> Bool
paramHasLowerBound name = any check
  where
    check (SAssert _ cond _) = exprHasLowerBound name cond
    check (SExpr e _) = exprIsAssertLowerBound name e
    check _ = False

    exprIsAssertLowerBound n (EApp (EApp (EVar "assertMsg" _) _ _) cond _) = exprHasLowerBound n cond
    exprIsAssertLowerBound _ _ = False

exprHasLowerBound :: Text -> Expr -> Bool
exprHasLowerBound name (EInfix ">=" (EVar v _) (ENum _ _) _) = v == name
exprHasLowerBound name (EInfix ">=" (EVar v _) (ELit _ _) _) = v == name
exprHasLowerBound name (EInfix "<=" (ENum _ _) (EVar v _) _) = v == name
exprHasLowerBound name (EInfix ">" (EVar v _) (ENum _ _) _) = v == name
exprHasLowerBound name (EInfix "&&" l r _) = exprHasLowerBound name l || exprHasLowerBound name r
exprHasLowerBound name (EParens e _) = exprHasLowerBound name e
exprHasLowerBound _ _ = False

paramHasUpperBound :: Text -> [Stmt] -> Bool
paramHasUpperBound name = any check
  where
    check (SAssert _ cond _) = exprHasUpperBound name cond
    check (SExpr e _) = exprIsAssertUpperBound name e
    check _ = False

    exprIsAssertUpperBound n (EApp (EApp (EVar "assertMsg" _) _ _) cond _) = exprHasUpperBound n cond
    exprIsAssertUpperBound _ _ = False

exprHasUpperBound :: Text -> Expr -> Bool
exprHasUpperBound name (EInfix "<=" (EVar v _) _rhs _) = v == name
exprHasUpperBound name (EInfix "<" (EVar v _) _rhs _) = v == name
exprHasUpperBound name (EInfix ">=" _lhs (EVar v _) _) = v == name
exprHasUpperBound name (EInfix ">" _lhs (EVar v _) _) = v == name
exprHasUpperBound name (EInfix "&&" l r _) = exprHasUpperBound name l || exprHasUpperBound name r
exprHasUpperBound name (EParens e _) = exprHasUpperBound name e
exprHasUpperBound _ _ = False

-- | Find template fields with only lower-bound in ensure clause.
-- Uses type-based detection (isNumericField) rather than name heuristics,
-- since template fields have type information available from the parser.
findFieldsWithOnlyLowerBoundInEnsure :: [Field] -> Expr -> [Text]
findFieldsWithOnlyLowerBoundInEnsure fields ensureExpr =
  [ fieldName f | f <- fields
  , exprHasLowerBound (fieldName f) ensureExpr
  , not (exprHasUpperBound (fieldName f) ensureExpr)
  , isNumericField f
  ]

-- | SPEC-INV-009: Asymmetric validation between sibling functions
--
-- Detects top-level functions that share a common "operation name" suffix
-- (after stripping verb prefixes like execute/abort/complete/cancel) but
-- have different sets of assertMsg messages.  If one function's assert
-- messages are a strict subset of another's, the function with fewer
-- assertions is missing invariant checks.
asymmetricSiblingValidation :: Inspection
asymmetricSiblingValidation = mkInspection
  "SPEC-INV-009"
  "Asymmetric validation between sibling functions"
  "Sibling functions (sharing an operation suffix) have asymmetric assertMsg checks — one is missing invariant validations present in the other."
  Warning
  [Invariant]
  $ \mod_ ->
    let funcs = [(name, body, loc) | DFunction name _ body loc <- moduleDecls mod_]
        -- Group functions by their operation suffix
        groups = groupBySuffix funcs
    in concatMap checkSiblingGroup (Map.elems groups)

-- | Action verb prefixes to strip when grouping sibling functions
actionPrefixes :: [Text]
actionPrefixes =
  [ "execute", "abort", "cancel", "complete", "start", "finish"
  , "initiate", "process", "confirm", "reject", "approve", "deny"
  , "begin", "end", "submit", "revoke", "accept", "decline"
  ]

-- | Strip a known verb prefix from a function name to get the operation suffix.
-- Returns Nothing if no prefix matches or the suffix would be empty.
stripActionPrefix :: Text -> Maybe Text
stripActionPrefix name =
  let results = [ suffix
                | prefix <- actionPrefixes
                , let nameL = T.toLower name
                , T.toLower prefix `T.isPrefixOf` nameL
                , let suffix = T.drop (T.length prefix) name
                , not (T.null suffix)
                ]
  in case results of
       (s:_) -> Just (T.toLower s)
       []    -> Nothing

-- | Group functions by their operation suffix
groupBySuffix :: [(Text, Expr, SrcSpan)] -> Map Text [(Text, Expr, SrcSpan)]
groupBySuffix funcs = Map.fromListWith (++)
  [ (suffix, [(name, body, loc)])
  | (name, body, loc) <- funcs
  , Just suffix <- [stripActionPrefix name]
  ]

-- | Extract assertMsg messages from an expression tree (for top-level functions)
collectAssertMsgsFromExpr :: Expr -> Set Text
collectAssertMsgsFromExpr = Set.fromList . go
  where
    go (EApp (EApp (EVar "assertMsg" _) msgExpr _) _ _) =
      case extractStringLiteral msgExpr of
        Just msg -> [msg]
        Nothing  -> []
    go (EApp f a _)     = go f ++ go a
    go (EParens e _)    = go e
    go (EIf _ t e _)    = go t ++ go e
    go (ECase _ alts _) = concatMap (go . snd) alts
    go (EDo stmts _)    = concatMap goStmt stmts
    go (ELet binds body _) = concatMap (go . bindExpr) binds ++ go body
    go (ELam _ body _)  = go body
    go _                = []

    goStmt (SAssert msg _ _)
      | not (T.null msg) = [msg]
    goStmt (SExpr e _)   = go e
    goStmt (SBind _ e _) = go e
    goStmt (SLet binds _) = concatMap (go . bindExpr) binds
    goStmt (SReturn e _) = go e
    goStmt _             = []

    extractStringLiteral (EString s _)  = Just s
    extractStringLiteral (EParens e _)  = extractStringLiteral e
    extractStringLiteral _              = Nothing

-- | Check a group of sibling functions for asymmetric validation.
-- If function A's asserts are a strict superset of function B's asserts,
-- flag B.
checkSiblingGroup :: [(Text, Expr, SrcSpan)] -> [Finding]
checkSiblingGroup group
  | length group < 2 = []
  | otherwise =
    let withMsgs = [ (name, collectAssertMsgsFromExpr body, loc)
                   | (name, body, loc) <- group ]
    in [ mkFinding (InspectionId "SPEC-INV-009") Warning locB
          ("Function '" <> nameB <> "' has " <> T.pack (show (Set.size msgsB))
           <> " assertMsg checks, but sibling function '" <> nameA <> "' has "
           <> T.pack (show (Set.size msgsA))
           <> " — missing: " <> T.intercalate ", " (map (\m -> "\"" <> m <> "\"") (Set.toList missing)))
          (Just $ "Add the missing assertion(s) to '" <> nameB <> "' for consistent invariant enforcement")
          Nothing
          Nothing
       | (nameA, msgsA, _locA) <- withMsgs
       , (nameB, msgsB, locB) <- withMsgs
       , nameA /= nameB
       , not (Set.null msgsA)         -- A must have some asserts
       , let missing = Set.difference msgsA msgsB
       , not (Set.null missing)       -- B must be missing some that A has
       , Set.isSubsetOf msgsB msgsA   -- B's asserts must be a subset of A's (strict)
       ]

-- | SPEC-INV-010: Missing cross-entity validation
--
-- Detects choices (or top-level functions) that fetch two or more contracts,
-- use fields from multiple fetched entities in a create statement, but
-- never assert a relationship between fields from different fetched entities.
-- This allows inconsistent data to be combined (e.g., a vault from a
-- different venue being paired with that venue).
missingCrossEntityValidation :: Inspection
missingCrossEntityValidation = mkInspection
  "SPEC-INV-010"
  "Missing cross-entity validation"
  "Multiple contracts are fetched and their fields are used together in a create, but no assertion checks consistency between the fetched entities."
  Warning
  [Invariant]
  $ \mod_ ->
    -- (1) In template choices
    [ mkFinding (InspectionId "SPEC-INV-010") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': fetches " <> T.pack (show (length fetchVars))
         <> " contracts and uses fields from multiple in a create, but never asserts cross-entity consistency")
        (Just "Add an assertion comparing fields from different fetched contracts, e.g., assertMsg \"mismatch\" (a.fieldX == b.fieldX)")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , let fetchVars = collectFetchVarsFromStmts (chBody ch)
    , length fetchVars >= 2
    , createUsesMultipleFetchVars fetchVars (chBody ch)
    , not (hasCrossEntityAssert fetchVars (chBody ch))
    ]
    ++
    -- (2) In top-level functions (with EDo body)
    [ mkFinding (InspectionId "SPEC-INV-010") Warning loc
        ("Function '" <> name
         <> "': fetches " <> T.pack (show (length fetchVars))
         <> " contracts and uses fields from multiple in a create, but never asserts cross-entity consistency")
        (Just "Add an assertion comparing fields from different fetched contracts, e.g., assertMsg \"mismatch\" (a.fieldX == b.fieldX)")
        Nothing
        Nothing
    | DFunction name _ body loc <- moduleDecls mod_
    , let stmts = exprToStmts body
    , let fetchVars = collectFetchVarsFromStmts stmts
    , length fetchVars >= 2
    , createUsesMultipleFetchVars fetchVars stmts
    , not (hasCrossEntityAssert fetchVars stmts)
    ]

-- | Extract statements from an expression (handles EDo wrapper)
exprToStmts :: Expr -> [Stmt]
exprToStmts (EDo stmts _) = stmts
exprToStmts _ = []

-- | Collect variable names bound to fetch results from a statement list.
-- Matches: x <- fetch someCid
collectFetchVarsFromStmts :: [Stmt] -> [Text]
collectFetchVarsFromStmts = concatMap go
  where
    go (SBind (PVar name _) expr _)
      | isFetchExpr' expr = [name]
    go (SFetch _ _) = []  -- SFetch without a bind doesn't give us a named var
    go _ = []

    isFetchExpr' (EApp (EVar "fetch" _) _ _) = True
    isFetchExpr' _ = False

-- | Check if any create statement in the body uses fields from 2+ different
-- fetch-bound variables.  We detect this by looking for dotted EVar names
-- like "venue.venueId" where the prefix before the first '.' matches a
-- fetch-bound variable.
createUsesMultipleFetchVars :: [Text] -> [Stmt] -> Bool
createUsesMultipleFetchVars fetchVars stmts = any checkStmt stmts
  where
    checkStmt (SCreate _ expr _) = usesMultiple expr
    checkStmt (SBind _ expr _) = usesMultipleInCreate expr
    checkStmt (SExpr expr _) = usesMultipleInCreate expr
    checkStmt _ = False

    -- Check if an expression that is a create uses multiple fetch vars
    usesMultipleInCreate (EApp (EVar "create" _) arg _) = usesMultiple arg
    usesMultipleInCreate (EApp f a _) = usesMultipleInCreate f || usesMultipleInCreate a
    usesMultipleInCreate _ = False

    -- Check if an expression (typically a record) references fields from 2+ fetch vars
    usesMultiple expr =
      let refs = collectDottedRefs expr
          matchedVars = Set.fromList [ fv | fv <- fetchVars, any (isDottedRefOf fv) refs ]
      in Set.size matchedVars >= 2

    -- Collect all dotted variable names from an expression.
    -- Handles both EVar "venue.venueId" (uppercase-qualified leftovers)
    -- and EFieldAccess (EVar "venue") "venueId" (from parser fix).
    collectDottedRefs :: Expr -> [Text]
    collectDottedRefs (EVar v _)
      | T.any (== '.') v = [v]
    collectDottedRefs (EFieldAccess (EVar base _) field _) = [base <> "." <> field]
    collectDottedRefs (EFieldAccess e _ _) = collectDottedRefs e
    collectDottedRefs (EApp f a _) = collectDottedRefs f ++ collectDottedRefs a
    collectDottedRefs (ERecordCon _ fields _) = concatMap (collectDottedRefs . snd) fields
    collectDottedRefs (ERecordUpd e fields _) = collectDottedRefs e ++ concatMap (collectDottedRefs . snd) fields
    collectDottedRefs (EParens e _) = collectDottedRefs e
    collectDottedRefs (EInfix _ l r _) = collectDottedRefs l ++ collectDottedRefs r
    collectDottedRefs (ETuple es _) = concatMap collectDottedRefs es
    collectDottedRefs (EList es _) = concatMap collectDottedRefs es
    collectDottedRefs _ = []

    -- Check if a dotted reference like "venue.venueId" refers to fetch var "venue"
    isDottedRefOf :: Text -> Text -> Bool
    isDottedRefOf fetchVar dottedRef =
      (fetchVar <> ".") `T.isPrefixOf` dottedRef

-- | Check if there is any assertion that compares fields from two different
-- fetch-bound variables (cross-entity validation).
--
-- We look for patterns like:
--   assertMsg "..." (x.field == y.field)
--   assert (x.field == y.field)
-- where x and y are different fetch-bound variables.
--
-- Because the parser may split assertMsg and its condition into separate
-- SExpr statements, we also check standalone expressions for cross-entity
-- comparisons.
hasCrossEntityAssert :: [Text] -> [Stmt] -> Bool
hasCrossEntityAssert fetchVars stmts = any checkStmt stmts
  where
    checkStmt (SAssert _ cond _) = exprHasCrossEntityCmp fetchVars cond
    checkStmt (SExpr e _) = exprHasCrossEntityCmp fetchVars e
    checkStmt (SBind _ e _) = exprHasCrossEntityCmp fetchVars e
    checkStmt _ = False

-- | Check if an expression contains a comparison between fields of two
-- different fetch-bound variables.
exprHasCrossEntityCmp :: [Text] -> Expr -> Bool
exprHasCrossEntityCmp fetchVars = go
  where
    go (EInfix op l r _)
      | op == "==" || op == "/=" =
        let lVars = fetchVarsReferenced fetchVars l
            rVars = fetchVarsReferenced fetchVars r
        in not (Set.null (Set.intersection lVars (Set.fromList fetchVars)))
           && not (Set.null (Set.intersection rVars (Set.fromList fetchVars)))
           && lVars /= rVars  -- different fetch vars on each side
      | otherwise = go l || go r
    go (EApp f a _) = go f || go a
    go (EParens e _) = go e
    go (EIf c t e _) = go c || go t || go e
    go (ECase e alts _) = go e || any (go . snd) alts
    go (EDo innerStmts _) = hasCrossEntityAssert fetchVars innerStmts
    go _ = False

-- | Determine which fetch-bound variables are referenced in an expression
-- (via dotted EVar names like "venue.venueId" or EFieldAccess nodes).
fetchVarsReferenced :: [Text] -> Expr -> Set Text
fetchVarsReferenced fetchVars = go
  where
    go (EVar v _) =
      Set.fromList [ fv | fv <- fetchVars, (fv <> ".") `T.isPrefixOf` v ]
    go (EFieldAccess (EVar base _) _ _) =
      Set.fromList [ fv | fv <- fetchVars, fv == base ]
    go (EFieldAccess e _ _) = go e
    go (EApp f a _) = Set.union (go f) (go a)
    go (EParens e _) = go e
    go (EInfix _ l r _) = Set.union (go l) (go r)
    go _ = Set.empty

-- | SPEC-INV-011: Time field not validated in ensure clause
--
-- A template with a Time-typed field (e.g., createdAt, expiresAt) should
-- typically validate that field in the ensure clause to prevent creation
-- of contracts with invalid time relationships (e.g., createdAt >= expiresAt).
-- If the ensure clause does not reference the time field at all (or there is
-- no ensure clause), this is a potential invariant gap.
timeFieldNotInEnsure :: Inspection
timeFieldNotInEnsure = mkInspection
  "SPEC-INV-011"
  "Time field not validated in ensure clause"
  "Template has a Time-typed field but the ensure clause does not reference it, potentially allowing contracts with invalid time relationships."
  Warning
  [Invariant]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INV-011") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl
         <> "' has Time field '" <> fieldName tf
         <> "' but the ensure clause does not validate it"
         <> " — contracts may be created with invalid time relationships")
        (Just $ "Add a check in the ensure clause that validates '" <> fieldName tf <> "' (e.g., ordering against other time fields or deadlines)")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , tf <- tplFields tpl
    , isTimeField tf
    , not (timeFieldReferencedInEnsure (fieldName tf) (tplEnsure tpl))
    ]

-- | Check if a field name is referenced in the ensure clause expression.
-- Returns True if the ensure clause references the given field name,
-- either directly or through dotted access.
timeFieldReferencedInEnsure :: Text -> Maybe Expr -> Bool
timeFieldReferencedInEnsure _ Nothing = False
timeFieldReferencedInEnsure name (Just expr) = exprRefsName name expr

-- | Check if an expression references a given name (directly, as prefix of
-- dotted name, or as the field name in EFieldAccess)
exprRefsName :: Text -> Expr -> Bool
exprRefsName name = go
  where
    go (EVar v _) = v == name || (name <> ".") `T.isPrefixOf` v
    go (EFieldAccess e field _) = field == name || go e
    go (EApp f a _) = go f || go a
    go (EParens e _) = go e
    go (EInfix _ l r _) = go l || go r
    go (EIf c t e _) = go c || go t || go e
    go (ECase s alts _) = go s || any (go . snd) alts
    go (EDo stmts _) = any goStmt stmts
    go (ELet binds body _) = any (\(Binding _ e _) -> go e) binds || go body
    go (EList es _) = any go es
    go (ETuple es _) = any go es
    go (ERecordCon _ fields _) = any (go . snd) fields
    go _ = False

    goStmt (SExpr e _) = go e
    goStmt (SBind _ e _) = go e
    goStmt (SAssert _ e _) = go e
    goStmt (SLet binds _) = any (\(Binding _ e _) -> go e) binds
    goStmt _ = False

-- | SPEC-INV-012: Indirect parameter assignment via Some/let without guard
--
-- Detects a choice parameter wrapped in @Some@ (via @let x = Some param@) and
-- then used in a @create this with { field = x }@, without any preceding
-- assertion of the form @param /= oldField@, @param /= controller@, or
-- @oldField == None@.  This allows redundant / no-op state transitions
-- (e.g., setting pending governor to the current governor).
-- Works on both template choices and standalone functions.
indirectParamAssignmentNoGuard :: Inspection
indirectParamAssignmentNoGuard = mkInspection
  "SPEC-INV-012"
  "Indirect parameter assignment without guard"
  "A choice parameter is wrapped in Some and assigned to a field via a let binding, but there is no assertion that the new value differs from the old or that the existing state is None."
  Warning
  [Invariant, StateIntegrity]
  $ \mod_ ->
    -- (1) Template choices
    [ mkFinding (InspectionId "SPEC-INV-012") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': parameter '" <> paramN
         <> "' is wrapped in Some (via '" <> letVar
         <> "') and assigned to a field without checking it differs from the current value or that the current state is None")
        (Just $ "Add 'assertMsg \"already set\" (" <> targetField <> " == None)' or 'assertMsg \"no change\" (" <> paramN <> " /= currentValue)' before the assignment")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , let paramNames = map fieldName (chParams ch)
    , (paramN, letVar, targetField) <- findIndirectSomeAssignments paramNames (chBody ch)
    , not (hasIndirectGuard paramN targetField (chBody ch))
    ]
    ++
    -- (2) Standalone functions
    [ mkFinding (InspectionId "SPEC-INV-012") Warning loc
        ("Function '" <> fname
         <> "': parameter '" <> paramN
         <> "' is wrapped in Some (via '" <> letVar
         <> "') and assigned to a field without checking it differs from the current value or that the current state is None")
        (Just $ "Add 'assertMsg \"already set\" (" <> targetField <> " == None)' or 'assertMsg \"no change\" (" <> paramN <> " /= currentValue)' before the assignment")
        Nothing
        Nothing
    | DFunction fname _ body loc <- moduleDecls mod_
    , let stmts = exprToStmts body
    , let paramNames = collectFuncParamNames stmts
    , (paramN, letVar, targetField) <- findIndirectSomeAssignments paramNames stmts
    , not (hasIndirectGuard paramN targetField stmts)
    ]

-- | Find (paramName, letVarName, targetFieldName) triples where:
--   1. @let letVar = Some paramName@
--   2. @create this with { targetField = letVar }@ (or via ERecordUpd)
findIndirectSomeAssignments :: [Text] -> [Stmt] -> [(Text, Text, Text)]
findIndirectSomeAssignments paramNames stmts =
  let -- Collect let bindings of the form: letVar = Some paramExpr
      someBindings = concatMap collectSomeLetBindings stmts
      -- Filter to those wrapping a known parameter
      paramSomeBinds = [ (pName, letVarN)
                       | (letVarN, wrappedName) <- someBindings
                       , pName <- paramNames
                       , wrappedName == pName
                       ]
      -- Find create-this-with field assignments using these let vars
      createFields = concatMap collectRecordUpdateFields stmts
  in [ (pName, letVarN, fldName)
     | (pName, letVarN) <- paramSomeBinds
     , (fldName, assignedVar) <- createFields
     , assignedVar == letVarN
     ]

-- | Collect @let x = Some y@ bindings -> (x, y)
collectSomeLetBindings :: Stmt -> [(Text, Text)]
collectSomeLetBindings (SLet binds _) =
  [ (bindName b, varName)
  | b <- binds
  , Just varName <- [extractSomeArg (bindExpr b)]
  ]
collectSomeLetBindings _ = []

-- | Extract the variable name from @Some x@ or @Just x@
extractSomeArg :: Expr -> Maybe Text
extractSomeArg (EApp (EVar "Some" _) (EVar name _) _) = Just name
extractSomeArg (EApp (EVar "Just" _) (EVar name _) _) = Just name
extractSomeArg _ = Nothing

-- | Collect (fieldName, assignedVarName) from @create this with { field = var }@
-- Both ERecordUpd and ERecordCon patterns.
collectRecordUpdateFields :: Stmt -> [(Text, Text)]
collectRecordUpdateFields (SCreate _ (ERecordUpd _ fields _) _) = extractVarFields fields
collectRecordUpdateFields (SBind _ expr _) = extractCreateVarFields expr
collectRecordUpdateFields (SExpr expr _) = extractCreateVarFields expr
collectRecordUpdateFields _ = []

extractCreateVarFields :: Expr -> [(Text, Text)]
extractCreateVarFields (EApp (EVar "create" _) (ERecordUpd _ fields _) _) = extractVarFields fields
extractCreateVarFields (EApp (EVar "create" _) (ERecordCon _ fields _) _) = extractVarFields fields
extractCreateVarFields (EApp f a _) = extractCreateVarFields f ++ extractCreateVarFields a
extractCreateVarFields (EInfix "<$>" _ b _) = extractCreateVarFields b
extractCreateVarFields (EParens e _) = extractCreateVarFields e
extractCreateVarFields _ = []

extractVarFields :: [(Text, Expr)] -> [(Text, Text)]
extractVarFields fields = [ (fName, v) | (fName, EVar v _) <- fields ]

-- | Check if there is a guard protecting the indirect assignment.
-- We look for:
--   1. @assertMsg ... (targetField == None)@ — checks current state is unset
--   2. @assertMsg ... (param /= someOtherValue)@ — checks param differs from current
--   3. Any @assertMsg@ mentioning the parameter name in its condition
hasIndirectGuard :: Text -> Text -> [Stmt] -> Bool
hasIndirectGuard paramN targetField stmts = any check stmts
  where
    check (SAssert _ cond _) =
      exprMentionsVar paramN cond
      || exprChecksNone targetField cond
    check (SExpr e _) = isAssertGuard e
    check _ = False

    isAssertGuard (EApp (EApp (EVar "assertMsg" _) _ _) cond _) =
      exprMentionsVar paramN cond
      || exprChecksNone targetField cond
    isAssertGuard (EApp f _ _) = isAssertGuard f
    isAssertGuard _ = False

-- | Check if an expression checks @field == None@
exprChecksNone :: Text -> Expr -> Bool
exprChecksNone field (EInfix "==" l r _) =
  (exprIsVar field l && isNoneExpr r) || (isNoneExpr l && exprIsVar field r)
exprChecksNone field (EInfix "&&" l r _) = exprChecksNone field l || exprChecksNone field r
exprChecksNone field (EParens e _) = exprChecksNone field e
exprChecksNone _ _ = False

exprIsVar :: Text -> Expr -> Bool
exprIsVar name (EVar v _) = v == name || name `T.isPrefixOf` v
exprIsVar _ _ = False

isNoneExpr :: Expr -> Bool
isNoneExpr (EVar "None" _) = True
isNoneExpr (EVar "Nothing" _) = True
isNoneExpr _ = False

-- | Heuristic: collect parameter names from destructuring let bindings
-- like @let Foo {bar, baz} = arg@ — we collect the field names.
-- Also treats any simple function parameter names (top-level names before =).
collectFuncParamNames :: [Stmt] -> [Text]
collectFuncParamNames stmts = concatMap go stmts
  where
    go (SLet binds _) = concatMap extractParamLike binds
    go _ = []

    extractParamLike (Binding name _ _)
      -- let Foo { field1, field2 } = arg  — extract field1, field2 from the record pattern
      | not (T.null name) = [name]
    extractParamLike _ = []

-- =========================================================================
-- SPEC-INV-013: Uncanonicalized list persisted after processing
-- =========================================================================

-- | SPEC-INV-013: Data flows to both a processing function and a create
-- without canonicalization.
--
-- Two detection strategies (no hardcoded function names):
--
-- (A) Choice-level (type-based): A list-typed parameter (from the choice's
--     or template's @with@ block) appears as an argument to a function call
--     AND in a record field of a @create@ statement, with no canonicalization.
--
-- (B) Standalone-function-level (structural): A record variable @v@ has a
--     field access @v.field@ passed as a function argument, AND @v@ itself
--     is stored in a @create@ record field, with no canonicalization.
--     This detects "sub-field extracted for processing but whole record
--     persisted unchanged."
--
-- The fix is to canonicalize the data before both processing and storage.
uncanonicalisedListPersist :: Inspection
uncanonicalisedListPersist = mkInspection
  "SPEC-INV-013"
  "Uncanonicalized list persisted in created contract"
  "Data flows to both a processing function and a create statement without canonicalization, creating a potential mismatch between processed and stored state."
  Warning
  [Invariant]
  $ \mod_ ->
    -- (A) Choices within templates — list-typed parameters
    [ mkFinding (InspectionId "SPEC-INV-013") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': list parameter '" <> listVar
         <> "' is passed to a function call and stored in a create without canonicalization")
        (Just "Canonicalize the list (e.g. List.dedup, sort) before persisting in the create statement")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , listVar <- collectListFieldNames (chParams ch ++ tplFields tpl)
    , varInFuncArgStmts listVar (chBody ch)
    , varInCreateFieldStmts listVar (chBody ch)
    , not (stmtsHaveCanonicalization (chBody ch))
    ]
    ++
    -- (B) Standalone functions — structural field-access pattern
    [ mkFinding (InspectionId "SPEC-INV-013") Warning loc
        ("Function '" <> fname
         <> "': variable '" <> sharedVar
         <> "' has a field passed to a function and is stored in a create without canonicalization")
        (Just "Canonicalize the relevant data (e.g. List.dedup, sort) before persisting in the create statement")
        Nothing
        Nothing
    | DFunction fname _ body loc <- moduleDecls mod_
    , sharedVar <- findFieldAccessPersistVars body
    , not (exprHasCanonicalization body)
    ]

-- | Check if a field type is a list type.
isListFieldType :: Maybe Type -> Bool
isListFieldType (Just (TList _ _)) = True
isListFieldType _ = False

-- | Collect names of list-typed fields.
collectListFieldNames :: [Field] -> [Text]
collectListFieldNames = map fieldName . filter (isListFieldType . fieldType)

-- | Check if a variable appears as an argument to a function call in statements.
varInFuncArgStmts :: Text -> [Stmt] -> Bool
varInFuncArgStmts var = any go
  where
    go (SBind _ e _)     = varInFuncArgExpr var e
    go (SExpr e _)       = varInFuncArgExpr var e
    go (SLet binds _)    = any (varInFuncArgExpr var . bindExpr) binds
    go (SReturn e _)     = varInFuncArgExpr var e
    go _                 = False

-- | Check if a variable appears as an argument to a function call in an expression.
-- In curried application @f x y@, both @x@ and @y@ are in argument positions.
varInFuncArgExpr :: Text -> Expr -> Bool
varInFuncArgExpr var (EApp f arg _)
  | exprMentionsVar var arg = True
  | otherwise = varInFuncArgExpr var f || varInFuncArgExpr var arg
varInFuncArgExpr var (EInfix _ l r _)    = varInFuncArgExpr var l || varInFuncArgExpr var r
varInFuncArgExpr var (EParens e _)       = varInFuncArgExpr var e
varInFuncArgExpr var (EDo stmts _)       = varInFuncArgStmts var stmts
varInFuncArgExpr var (EIf c t e _)       = varInFuncArgExpr var c || varInFuncArgExpr var t || varInFuncArgExpr var e
varInFuncArgExpr var (ECase _ alts _)    = any (varInFuncArgExpr var . snd) alts
varInFuncArgExpr var (ELam _ body _)     = varInFuncArgExpr var body
varInFuncArgExpr var (ELet binds body _) = any (varInFuncArgExpr var . bindExpr) binds || varInFuncArgExpr var body
varInFuncArgExpr _ _                     = False

-- | Check if a variable appears in a record field of a create statement.
varInCreateFieldStmts :: Text -> [Stmt] -> Bool
varInCreateFieldStmts var = any go
  where
    go (SCreate _ e _)   = varInRecordFields var e
    go (SBind _ e _)     = varInCreateFieldExpr var e
    go (SExpr e _)       = varInCreateFieldExpr var e
    go (SLet binds _)    = any (varInCreateFieldExpr var . bindExpr) binds
    go (SReturn e _)     = varInCreateFieldExpr var e
    go _                 = False

-- | Check if a variable appears in a create's record fields within an expression.
varInCreateFieldExpr :: Text -> Expr -> Bool
varInCreateFieldExpr var (EApp f arg _)
  | isCreateHead f = varInRecordFields var arg || varInCreateFieldExpr var arg
  | otherwise      = varInCreateFieldExpr var f || varInCreateFieldExpr var arg
varInCreateFieldExpr var (EDo stmts _)       = varInCreateFieldStmts var stmts
varInCreateFieldExpr var (EIf _ t e _)       = varInCreateFieldExpr var t || varInCreateFieldExpr var e
varInCreateFieldExpr var (ECase _ alts _)    = any (varInCreateFieldExpr var . snd) alts
varInCreateFieldExpr var (ELam _ body _)     = varInCreateFieldExpr var body
varInCreateFieldExpr var (ELet binds body _) = any (varInCreateFieldExpr var . bindExpr) binds || varInCreateFieldExpr var body
varInCreateFieldExpr var (EInfix _ l r _)    = varInCreateFieldExpr var l || varInCreateFieldExpr var r
varInCreateFieldExpr var (EParens e _)       = varInCreateFieldExpr var e
varInCreateFieldExpr _ _                     = False

-- | Check if a variable appears in the fields of a record expression.
varInRecordFields :: Text -> Expr -> Bool
varInRecordFields var (ERecordCon _ flds _) = any (exprMentionsVar var . snd) flds
varInRecordFields var (ERecordUpd _ flds _) = any (exprMentionsVar var . snd) flds
varInRecordFields _ _                       = False

-- | Check if an expression is a @create@ call head (possibly curried with type arg).
isCreateHead :: Expr -> Bool
isCreateHead (EVar "create" _) = True
isCreateHead (EApp f _ _)      = isCreateHead f
isCreateHead (EParens e _)     = isCreateHead e
isCreateHead _                 = False

-- | For standalone functions: find variables where @v.field@ is passed as a
-- function argument AND @v@ is stored in a create record field.
findFieldAccessPersistVars :: Expr -> [Text]
findFieldAccessPersistVars body =
  let fieldAccessBases = Set.fromList (extractFieldAccessBasesInArgs body)
      createVars       = Set.fromList (extractCreateRecordVars body)
  in Set.toList (Set.intersection fieldAccessBases createVars)

-- | Extract base variable names from field accesses in function argument positions.
-- E.g. in @f transfer.inputHoldingCids@, extracts @"transfer"@.
extractFieldAccessBasesInArgs :: Expr -> [Text]
extractFieldAccessBasesInArgs = goExpr
  where
    goExpr (EApp f arg _)    = extractFieldAccessBase arg ++ goExpr f ++ goExpr arg
    goExpr (EInfix _ l r _)  = goExpr l ++ goExpr r
    goExpr (EParens e _)     = goExpr e
    goExpr (EDo stmts _)     = concatMap goStmt stmts
    goExpr (EIf c t e _)     = goExpr c ++ goExpr t ++ goExpr e
    goExpr (ECase _ alts _)  = concatMap (goExpr . snd) alts
    goExpr (ELam _ body _)   = goExpr body
    goExpr (ELet binds body _) = concatMap (goExpr . bindExpr) binds ++ goExpr body
    goExpr _                 = []

    goStmt (SBind _ e _)     = goExpr e
    goStmt (SExpr e _)       = goExpr e
    goStmt (SLet binds _)    = concatMap (goExpr . bindExpr) binds
    goStmt (SReturn e _)     = goExpr e
    goStmt _                 = []

-- | Extract the base variable name from field access expressions.
-- @transfer.inputHoldingCids@ → @["transfer"]@.
-- Note: the parser's 'pVarOrCon' greedily consumes dots, so
-- @transfer.requestedAt@ is often parsed as @EVar "transfer.requestedAt"@
-- rather than @EFieldAccess (EVar "transfer") "requestedAt"@.  We handle
-- both representations.
extractFieldAccessBase :: Expr -> [Text]
extractFieldAccessBase (EFieldAccess (EVar base _) _ _) = [base]
extractFieldAccessBase (EFieldAccess base _ _)           = extractFieldAccessBase base
extractFieldAccessBase (EVar name _)
  | (base, rest) <- T.breakOn "." name
  , not (T.null rest) = [base]
extractFieldAccessBase (EApp f a _)                      = extractFieldAccessBase f ++ extractFieldAccessBase a
extractFieldAccessBase (EParens e _)                     = extractFieldAccessBase e
extractFieldAccessBase _                                 = []

-- | Extract variable names from record fields of create expressions.
-- Handles both @SCreate@ statements and expression-level creates.
extractCreateRecordVars :: Expr -> [Text]
extractCreateRecordVars = goExpr
  where
    goExpr (EApp f arg _)
      | isCreateHead f = extractRecVarNames arg ++ goExpr arg
      | otherwise      = goExpr f ++ goExpr arg
    goExpr (EInfix _ l r _)    = goExpr l ++ goExpr r
    goExpr (EParens e _)       = goExpr e
    goExpr (EDo stmts _)       = concatMap goStmt stmts
    goExpr (EIf _ t e _)       = goExpr t ++ goExpr e
    goExpr (ECase _ alts _)    = concatMap (goExpr . snd) alts
    goExpr (ELam _ body _)     = goExpr body
    goExpr (ELet binds body _) = concatMap (goExpr . bindExpr) binds ++ goExpr body
    goExpr _                   = []

    goStmt (SCreate _ e _)   = extractRecVarNames e
    goStmt (SBind _ e _)     = goExpr e
    goStmt (SExpr e _)       = goExpr e
    goStmt (SLet binds _)    = concatMap (goExpr . bindExpr) binds
    goStmt (SReturn e _)     = goExpr e
    goStmt _                 = []

-- | Extract top-level variable names from a record expression's fields.
extractRecVarNames :: Expr -> [Text]
extractRecVarNames (ERecordCon _ flds _) = concatMap (topVarName . snd) flds
extractRecVarNames (ERecordUpd _ flds _) = concatMap (topVarName . snd) flds
extractRecVarNames _                     = []

-- | Get the top-level variable name from a simple expression.
topVarName :: Expr -> [Text]
topVarName (EVar name _)                    = [name]
topVarName (EFieldAccess (EVar base _) _ _) = [base]
topVarName (EParens e _)                    = topVarName e
topVarName _                                = []

-- | Canonicalization function names.
isCanonicalizeFunc :: Text -> Bool
isCanonicalizeFunc name =
  name `elem`
    [ "List.dedup", "dedup", "nub", "List.nub"
    , "Set.fromList", "Set.toList"
    , "List.unique", "unique"
    , "List.sort", "sort"
    ]

-- | Check if statements contain a canonicalization call.
stmtsHaveCanonicalization :: [Stmt] -> Bool
stmtsHaveCanonicalization = any go
  where
    go (SBind _ e _)     = exprHasCanonicalization e
    go (SExpr e _)       = exprHasCanonicalization e
    go (SLet binds _)    = any (exprHasCanonicalization . bindExpr) binds
    go (SReturn e _)     = exprHasCanonicalization e
    go _                 = False

-- | Check if an expression contains a canonicalization call.
exprHasCanonicalization :: Expr -> Bool
exprHasCanonicalization (EVar name _) = isCanonicalizeFunc name
exprHasCanonicalization (EApp f a _) =
  exprHasCanonicalization f || exprHasCanonicalization a
exprHasCanonicalization (EInfix _ l r _) =
  exprHasCanonicalization l || exprHasCanonicalization r
exprHasCanonicalization (EParens e _) = exprHasCanonicalization e
exprHasCanonicalization (EDo stmts _) = stmtsHaveCanonicalization stmts
exprHasCanonicalization (EIf c t e _) =
  exprHasCanonicalization c || exprHasCanonicalization t || exprHasCanonicalization e
exprHasCanonicalization (ECase _ alts _) =
  any (exprHasCanonicalization . snd) alts
exprHasCanonicalization (ELam _ body _) = exprHasCanonicalization body
exprHasCanonicalization (ELet binds body _) =
  any (exprHasCanonicalization . bindExpr) binds || exprHasCanonicalization body
exprHasCanonicalization _ = False

-- | SPEC-INV-014: Unsafe division without zero check
unsafeDivision :: Inspection
unsafeDivision = mkInspection
  "SPEC-INV-014"
  "Unsafe division without zero check"
  "Division operation using a variable denominator without prior validation against zero. Can lead to transaction aborts (DoS)."
  Warning
  [Invariant]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INV-014") Warning loc
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': division by variable '" <> denom <> "' without prior zero check")
        (Just $ "Add 'assertMsg \"...\" (" <> denom <> " /= 0.0)' before division")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , (loc, denom) <- findUnsafeDivisions (chBody ch)
    ]

findUnsafeDivisions :: [Stmt] -> [(SrcSpan, Text)]
findUnsafeDivisions stmts =
  let divs = extractDivisions stmts
      guards = extractDenominatorGuards stmts
  in [(loc, denom) | (loc, denom) <- divs, not (denom `Set.member` guards)]

extractDivisions :: [Stmt] -> [(SrcSpan, Text)]
extractDivisions = concatMap stmtDivs
  where
    stmtDivs (SExpr e _) = exprDivs e
    stmtDivs (SBind _ e _) = exprDivs e
    stmtDivs (SLet binds _) = concatMap (exprDivs . bindExpr) binds
    stmtDivs (SCreate _ e _) = exprDivs e
    stmtDivs (SReturn e _) = exprDivs e
    stmtDivs (SAssert _ e _) = exprDivs e
    stmtDivs _ = []

    exprDivs (EInfix "/" _ (EVar denom _) s) = [(s, denom)]
    exprDivs (EInfix _ l r _) = exprDivs l ++ exprDivs r
    exprDivs (EApp f a _) = exprDivs f ++ exprDivs a
    exprDivs (EParens e _) = exprDivs e
    exprDivs (EIf c t e _) = exprDivs c ++ exprDivs t ++ exprDivs e
    exprDivs (ECase e alts _) = exprDivs e ++ concatMap (exprDivs . snd) alts
    exprDivs (ELet binds body _) = concatMap (exprDivs . bindExpr) binds ++ exprDivs body
    exprDivs (EDo stmts _) = extractDivisions stmts
    exprDivs _ = []

-- | Extract variables that are guarded by any assertion-like statement.
-- The approach is general: any assertion (assertMsg, assert, require, when/unless)
-- whose condition expression mentions a variable counts as a guard for that variable.
-- This covers literal zero checks (v > 0), named-constant checks (v > minDenom),
-- and helper-function guards (assertPositive v).
extractDenominatorGuards :: [Stmt] -> Set Text
extractDenominatorGuards stmts = Set.fromList $ concatMap stmtGuards stmts
  where
    stmtGuards (SAssert _ cond _) = extractMentionedVars cond
    stmtGuards (SExpr e _) = assertCondVars e
    stmtGuards _ = []

    -- Unwrap assertMsg/assert/require to get the condition, then extract vars
    assertCondVars (EApp (EApp (EVar fn _) _ _) cond _)
      | fn `elem` ["assertMsg", "require"] = extractMentionedVars cond
    assertCondVars (EApp (EVar "assert" _) cond _) = extractMentionedVars cond
    assertCondVars _ = []

    -- Extract all variable names mentioned in a comparison expression.
    -- We only count variables that appear in comparison operators (>, <, >=, <=, /=, ==)
    -- to avoid over-broadly suppressing from unrelated assertions.
    extractMentionedVars (EInfix op (EVar v _) _ _)
      | op `elem` [">", "<", ">=", "<=", "/=", "=="] = [v]
    extractMentionedVars (EInfix op _ (EVar v _) _)
      | op `elem` [">", "<", ">=", "<=", "/=", "=="] = [v]
    extractMentionedVars (EInfix "&&" l r _) = extractMentionedVars l ++ extractMentionedVars r
    extractMentionedVars (EInfix "||" l r _) = extractMentionedVars l ++ extractMentionedVars r
    extractMentionedVars (EApp (EVar _ _) (EVar v _) _) = [v]  -- function call like: assertPositive v
    extractMentionedVars (EParens e _) = extractMentionedVars e
    extractMentionedVars (EApp (EApp (EVar "not" _) inner _) _ _) = extractMentionedVars inner
    extractMentionedVars (EApp (EVar "not" _) inner _) = extractMentionedVars inner
    extractMentionedVars _ = []
