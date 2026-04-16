{-# LANGUAGE OverloadedStrings #-}
-- | Lifecycle-related inspection rules.
--
-- Checks for nonconsuming choices that create contracts and other lifecycle issues.
module Spectre.Rules.Lifecycle
  ( lifecycleRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Spectre.Ast
import Spectre.Inspection

-- | All lifecycle rules
lifecycleRules :: [Inspection]
lifecycleRules =
  [ nonconsumingCreatesContract
  , missingRevocation
  , splitWithoutRemainder
  , templateTransmutation
  ]

-- | SPEC-LIFE-001: NonConsuming choice creates contracts without guard
--
-- A nonconsuming choice that creates new contracts can be exercised
-- repeatedly, potentially leading to unbounded contract creation.
-- Such choices should have guards to prevent re-execution.
nonconsumingCreatesContract :: Inspection
nonconsumingCreatesContract = mkInspection
  "SPEC-LIFE-001"
  "NonConsuming choice creates contracts"
  "A nonconsuming choice creates new contracts but has no guard against repeated execution. This could lead to unbounded contract creation."
  Warning
  [Lifecycle]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-LIFE-001") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "' is nonconsuming but creates contracts without a re-execution guard")
        (Just "Add a guard (e.g., check a flag or use a key) to prevent repeated execution, or make the choice consuming")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , chConsuming ch == NonConsuming
    , bodyCreatesContract (chBody ch)
    , not (bodyHasGuard (chBody ch))
    ]

-- Helpers

bodyCreatesContract :: [Stmt] -> Bool
bodyCreatesContract = any isCreate
  where
    isCreate (SCreate _ _ _) = True
    isCreate (SExpr e _) = exprHasCreate e
    isCreate (SBind _ e _) = exprHasCreate e
    isCreate _ = False

    exprHasCreate (EApp (EVar "create" _) _ _) = True
    exprHasCreate (EApp f a _) = exprHasCreate f || exprHasCreate a
    exprHasCreate _ = False

bodyHasGuard :: [Stmt] -> Bool
bodyHasGuard stmts = any isGuard stmts
  where
    isGuard (SAssert _ _ _) = True
    isGuard (SExpr (EApp (EVar "assertMsg" _) _ _) _) = True
    isGuard (SExpr (EApp (EApp (EVar "assertMsg" _) _ _) _ _) _) = True
    isGuard _ = False

-- | SPEC-LIFE-002: Multi-signatory template without consuming choices
--
-- A template with two or more signatories but no consuming choices can only
-- be archived via the implicit Archive, which requires ALL signatories.
-- This means no single party can unilaterally revoke or terminate the
-- contract, which is often a security/lifecycle gap.
missingRevocation :: Inspection
missingRevocation = mkInspection
  "SPEC-LIFE-002"
  "Multi-signatory template lacks consuming choice"
  "Template with multiple signatories has no consuming choice. Archive requires unanimous consent, preventing unilateral revocation."
  Warning
  [Lifecycle]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-LIFE-002") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl
         <> "' has " <> T.pack (show (length (tplSignatory tpl)))
         <> " signatories but no consuming choice — only implicit Archive is available")
        (Just "Add an explicit consuming choice that allows authorized parties to revoke/terminate the contract unilaterally")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , length (tplSignatory tpl) >= 2
    , not (any isConsumingChoice (tplChoices tpl))
    ]

-- | Check if a choice is consuming (default = consuming)
isConsumingChoice :: Choice -> Bool
isConsumingChoice ch = chConsuming ch == Consuming || chConsuming ch == PreConsuming || chConsuming ch == PostConsuming

-- | SPEC-LIFE-003: Split/partial-fill without remainder handling
--
-- A consuming choice that:
--   1. Asserts field >= param (sufficient balance check)
--   2. Creates 'this with field = param' (partial output)
--   3. Does NOT conditionally handle the case where field > param
--      (no remainder create / no if-then-else checking for leftover)
--
-- This pattern effectively burns the difference (field - param) because
-- the original contract is archived (consumed) but only a contract for
-- 'param' is created.
splitWithoutRemainder :: Inspection
splitWithoutRemainder = mkInspection
  "SPEC-LIFE-003"
  "Split without remainder handling"
  "A consuming choice archives a contract and re-creates it with a partial amount but does not handle the remainder. The difference is effectively lost."
  Warning
  [Lifecycle]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-LIFE-003") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': asserts '" <> tplField <> " >= " <> chParam
         <> "' and creates with '" <> tplField <> " = " <> chParam
         <> "' but does not handle the remainder when " <> tplField
         <> " > " <> chParam <> " — the difference is lost")
        (Just $ "Add remainder handling: 'if " <> tplField <> " > " <> chParam
                <> " then create this with " <> tplField <> " = " <> tplField
                <> " - " <> chParam <> "'")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , isConsumingChoice ch
    , (tplField, chParam) <- findGeAssertPairs (tplFields tpl) (chParams ch) (chBody ch)
    -- Check there's a create with field = param
    , bodyCreatesWithFieldEqParam tplField chParam (chBody ch)
    -- Check there's NO remainder handling (no if field > param or field /= param)
    , not (bodyHasRemainderCheck tplField chParam (chBody ch))
    ]

-- | Find pairs of (templateField, choiceParam) where the body asserts
-- templateField >= choiceParam.
findGeAssertPairs :: [Field] -> [Field] -> [Stmt] -> [(Text, Text)]
findGeAssertPairs tplFields_ chParams_ stmts =
  let tplNames = map fieldName tplFields_
      chNames  = map fieldName chParams_
  in [ (tf, cp)
     | stmt <- stmts
     , (lhs, rhs) <- extractGeComparisons stmt
     , (tf, cp) <- matchFieldParam tplNames chNames lhs rhs
     ]

-- | Extract >= comparisons from a statement
extractGeComparisons :: Stmt -> [(Text, Text)]
extractGeComparisons (SAssert _ expr _) = extractGeFromExpr expr
extractGeComparisons (SExpr expr _) = extractGeFromExpr expr
extractGeComparisons _ = []

extractGeFromExpr :: Expr -> [(Text, Text)]
extractGeFromExpr expr = case expr of
  EInfix ">=" (EVar l _) (EVar r _) _ -> [(l, r)]
  EInfix "<=" (EVar l _) (EVar r _) _ -> [(r, l)]  -- a <= b means b >= a
  -- Handle parenthesized variables
  EInfix ">=" l r _ -> [(lv, rv) | lv <- getVarName l, rv <- getVarName r]
  EInfix "<=" l r _ -> [(rv, lv) | lv <- getVarName l, rv <- getVarName r]
  EApp f a _ -> extractGeFromExpr f ++ extractGeFromExpr a
  EParens e _ -> extractGeFromExpr e
  _ -> []

-- | Try to extract a simple variable name from an expression,
-- looking through parentheses.
getVarName :: Expr -> [Text]
getVarName (EVar v _) = [v]
getVarName (EParens e _) = getVarName e
getVarName _ = []

-- | Match extracted variable names against template fields and choice params
matchFieldParam :: [Text] -> [Text] -> Text -> Text -> [(Text, Text)]
matchFieldParam tplNames chNames lhs rhs =
  [ (lhs, rhs) | lhs `elem` tplNames, rhs `elem` chNames ]

-- | Check if the body creates 'this with field = param'
bodyCreatesWithFieldEqParam :: Text -> Text -> [Stmt] -> Bool
bodyCreatesWithFieldEqParam tplField chParam = any check
  where
    check (SCreate _ expr _) = exprHasFieldAssign tplField chParam expr
    check (SExpr expr _) = exprCreatesWithField tplField chParam expr
    check (SBind _ expr _) = exprCreatesWithField tplField chParam expr
    check _ = False

    exprCreatesWithField tf cp (EApp (EVar "create" _) arg _) =
      exprHasFieldAssign tf cp arg
    exprCreatesWithField tf cp (EApp f a _) =
      exprCreatesWithField tf cp f || exprCreatesWithField tf cp a
    exprCreatesWithField tf cp (EParens e _) = exprCreatesWithField tf cp e
    exprCreatesWithField _ _ _ = False

    exprHasFieldAssign tf cp (ERecordUpd _ fields _) =
      any (\(f, e) -> f == tf && isVarNamed cp e) fields
    exprHasFieldAssign tf cp (ERecordCon _ fields _) =
      any (\(f, e) -> f == tf && isVarNamed cp e) fields
    exprHasFieldAssign tf cp (EApp _ arg _) = exprHasFieldAssign tf cp arg
    exprHasFieldAssign tf cp (EParens e _) = exprHasFieldAssign tf cp e
    exprHasFieldAssign _ _ _ = False

    isVarNamed name (EVar v _) = v == name
    isVarNamed name (EParens e _) = isVarNamed name e
    isVarNamed _ _ = False

-- | Check if the body has remainder-handling logic:
-- An if/case comparing the template field against the choice param
-- (field > param, field /= param, field == param, etc.)
bodyHasRemainderCheck :: Text -> Text -> [Stmt] -> Bool
bodyHasRemainderCheck tplField chParam = any check
  where
    check (SExpr expr _) = exprHasRemainderCheck expr
    check (SBind _ expr _) = exprHasRemainderCheck expr
    check (SLet binds _) = any (exprHasRemainderCheck . bindExpr) binds
    check (SReturn expr _) = exprHasRemainderCheck expr
    check _ = False

    exprHasRemainderCheck expr = case expr of
      -- if field > param then ... or if field /= param then ...
      EIf cond _ _ _ -> exprComparesVars tplField chParam cond
      -- case expression comparing field and param
      ECase scrutinee _ _ -> exprComparesVars tplField chParam scrutinee
      EApp f a _ -> exprHasRemainderCheck f || exprHasRemainderCheck a
      EParens e _ -> exprHasRemainderCheck e
      EDo stmts _ -> any check stmts
      _ -> False

    exprComparesVars f p expr = case expr of
      EInfix op (EVar l _) (EVar r _) _
        | op `elem` [">", "<", "/=", "==", ">=", "<="] ->
            (l == f && r == p) || (l == p && r == f)
      EApp fn a _ -> exprComparesVars f p fn || exprComparesVars f p a
      EParens e _ -> exprComparesVars f p e
      _ -> False

-- | SPEC-LIFE-004: Template transmutation — consuming choice creates different template
--
-- A consuming choice that creates a contract of a different template type
-- effectively "transmutes" the contract. Any other contracts holding a
-- ContractId reference to the original template will have dangling references
-- because the original contract is archived but no contract of the same type
-- replaces it.
templateTransmutation :: Inspection
templateTransmutation = mkInspection
  "SPEC-LIFE-004"
  "Consuming choice creates different template type"
  "A consuming choice archives the current contract and creates a contract of a different template type. ContractId references from other contracts will become dangling."
  Warning
  [Lifecycle]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-LIFE-004") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "' is consuming and creates '" <> createdTpl
         <> "' (different type) — ContractId references to '" <> tplName tpl
         <> "' from other contracts will become dangling")
        (Just "Ensure all contracts referencing this template are also archived or updated, or use a non-consuming pattern")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , isConsumingChoice ch
    , createdTpl <- findCreatedTemplateNames (chBody ch)
    , createdTpl /= tplName tpl
    -- Don't flag if the choice also creates a contract of the same type (replacement)
    , not (tplName tpl `elem` findCreatedTemplateNames (chBody ch))
    ]

-- | Extract template names from create statements in a choice body.
findCreatedTemplateNames :: [Stmt] -> [Text]
findCreatedTemplateNames = concatMap extractFromStmt
  where
    extractFromStmt (SCreate (TCon tname _) _ _) = [tname]
    extractFromStmt (SCreate _ _ _) = []
    extractFromStmt (SExpr expr _) = extractFromExpr expr
    extractFromStmt (SBind _ expr _) = extractFromExpr expr
    extractFromStmt (SReturn expr _) = extractFromExpr expr
    extractFromStmt (SLet binds _) = concatMap (extractFromExpr . bindExpr) binds
    extractFromStmt _ = []

    extractFromExpr (EApp (EVar "create" _) arg _) = templateNameFromArg arg
    extractFromExpr (EApp f a _) = extractFromExpr f ++ extractFromExpr a
    extractFromExpr (EParens e _) = extractFromExpr e
    extractFromExpr (EDo stmts _) = concatMap extractFromStmt stmts
    extractFromExpr _ = []

    -- Extract template name from the argument to create:
    --   create Foo with { ... }  → ERecordCon "Foo" ...
    templateNameFromArg (ERecordCon name _ _) = [name]
    templateNameFromArg (EVar name _)
      | not (T.null name) && isUpperFirst name = [name]
    templateNameFromArg (EApp f _ _) = templateNameFromArg f
    templateNameFromArg (EParens e _) = templateNameFromArg e
    templateNameFromArg _ = []

    isUpperFirst t = case T.uncons t of
      Just (c, _) -> c >= 'A' && c <= 'Z'
      Nothing -> False
