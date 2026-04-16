{-# LANGUAGE OverloadedStrings #-}
-- | Scalability-related inspection rules.
--
-- Checks for unbounded collection growth.
module Spectre.Rules.Scalability
  ( scalabilityRules
  ) where

import Data.Text (Text)

import Spectre.Ast
import Spectre.Inspection

-- | All scalability rules
scalabilityRules :: [Inspection]
scalabilityRules =
  [ unboundedCollectionGrowth
  , eventAsContractBloat
  , unboundedIterationCreate
  ]

-- | SPEC-SCALE-001: Unbounded collection growth
--
-- Detects templates with list or set fields where choices only ever
-- add elements but never remove them, leading to unbounded growth.
unboundedCollectionGrowth :: Inspection
unboundedCollectionGrowth = mkInspection
  "SPEC-SCALE-001"
  "Unbounded collection growth"
  "Template has a collection field (list/set) that is only ever appended to but never pruned. This will grow without bound over time."
  Warning
  [Scalability]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-SCALE-001") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl
         <> "': collection field '" <> fieldName f
         <> "' appears to only grow (append/insert/cons) without any pruning")
        (Just "Add a mechanism to prune or bound the collection size")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , f <- tplFields tpl
    , isCollectionType (fieldType f)
    , fieldGrowsOnly (fieldName f) (tplChoices tpl)
    ]

-- Helpers

isCollectionType :: Maybe Type -> Bool
isCollectionType Nothing = False
isCollectionType (Just ty) = case ty of
  TList _ _ -> True
  TApp (TCon name _) _ _ ->
    name `elem` ["Set", "Map", "DA.Set", "DA.Map", "Set.Set", "Map.Map"]
  TCon name _ ->
    name `elem` ["Set", "Map", "DA.Set", "DA.Map"]
  _ -> False

-- | Check if a field is only ever appended to
fieldGrowsOnly :: Text -> [Choice] -> Bool
fieldGrowsOnly fieldName choices =
  let allBodies = concatMap chBody choices
      hasGrow = any (stmtGrowsField fieldName) allBodies
      hasShrink = any (stmtShrinksField fieldName) allBodies
  in hasGrow && not hasShrink

stmtGrowsField :: Text -> Stmt -> Bool
stmtGrowsField name (SCreate _ expr _) = exprGrowsField name expr
stmtGrowsField name (SExpr expr _) = exprGrowsField name expr
stmtGrowsField name (SBind _ expr _) = exprGrowsField name expr
stmtGrowsField name (SLet binds _) = any (exprGrowsField name . bindExpr) binds
stmtGrowsField _ _ = False

exprGrowsField :: Text -> Expr -> Bool
exprGrowsField name expr = case expr of
  -- Set.insert x field or field `Set.insert` x
  EApp (EApp (EVar func _) _ _) (EVar v _) _
    | v == name && isInsertFunc func -> True
  EApp (EApp (EVar func _) _ _) (EFieldAccess _ v _) _
    | v == name && isInsertFunc func -> True
  -- field ++ [x] or x : field
  EInfix "++" (EVar v _) _ _
    | v == name -> True
  EInfix "++" _ (EVar v _) _
    | v == name -> True
  EInfix ":" _ (EVar v _) _
    | v == name -> True
  -- Record constructor with field = ... insert/cons
  ERecordCon _ fields _ ->
    any (\(f, e) -> f == name && (isInsertExpr e || isConsExpr e)) fields
  -- Record update (this with { field = ... insert/cons })
  ERecordUpd _ fields _ ->
    any (\(f, e) -> f == name && (isInsertExpr e || isConsExpr e)) fields
  EApp f a _ -> exprGrowsField name f || exprGrowsField name a
  EParens e _ -> exprGrowsField name e
  _ -> False

stmtShrinksField :: Text -> Stmt -> Bool
stmtShrinksField name (SCreate _ expr _) = exprShrinksField name expr
stmtShrinksField name (SExpr expr _) = exprShrinksField name expr
stmtShrinksField name (SBind _ expr _) = exprShrinksField name expr
stmtShrinksField _ _ = False

exprShrinksField :: Text -> Expr -> Bool
exprShrinksField name expr = case expr of
  EApp (EApp (EVar func _) _ _) (EVar v _) _
    | v == name && isDeleteFunc func -> True
  EApp (EApp (EVar func _) _ _) (EFieldAccess _ v _) _
    | v == name && isDeleteFunc func -> True
  EApp f a _ -> exprShrinksField name f || exprShrinksField name a
  EParens e _ -> exprShrinksField name e
  ERecordCon _ fields _ ->
    any (\(f, e) -> f == name && isDeleteExpr e) fields
  ERecordUpd _ fields _ ->
    any (\(f, e) -> f == name && isDeleteExpr e) fields
  _ -> False

isInsertFunc :: Text -> Bool
isInsertFunc f = f `elem` ["Set.insert", "Map.insert", "DA.Set.insert", "DA.Map.insert", "insert"]

isDeleteFunc :: Text -> Bool
isDeleteFunc f = f `elem` ["Set.delete", "Map.delete", "DA.Set.delete", "DA.Map.delete", "delete", "Set.filter", "Map.filter", "filter"]

isInsertExpr :: Expr -> Bool
isInsertExpr (EApp (EVar f _) _ _) = isInsertFunc f
isInsertExpr (EApp f _ _) = isInsertExpr f
isInsertExpr _ = False

isConsExpr :: Expr -> Bool
isConsExpr (EInfix ":" _ _ _) = True
isConsExpr (EInfix "++" _ _ _) = True
isConsExpr _ = False

isDeleteExpr :: Expr -> Bool
isDeleteExpr (EApp (EVar f _) _ _) = isDeleteFunc f
isDeleteExpr (EApp f _ _) = isDeleteExpr f
isDeleteExpr _ = False

-- | SPEC-SCALE-002: Event-as-contract bloat
--
-- Detects templates with no choices.  Such templates can only be archived
-- by signatories (not through workflow choices), so when created as
-- event/audit records they accumulate in the active contract set without
-- bound.  Implementing an interface view alone does not add operational
-- choices.  The recommended DAML pattern is to use a data type returned
-- from a choice, or a nonconsuming choice for event emission.
eventAsContractBloat :: Inspection
eventAsContractBloat = mkInspection
  "SPEC-SCALE-002"
  "Event-as-contract bloat"
  "Template has no choices. Contracts of this type can only be archived by signatories and will accumulate in the active contract set."
  Warning
  [Scalability]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-SCALE-002") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl
         <> "' has no choices — contracts will accumulate in the active contract set")
        (Just "Consider using a data type returned from a choice, or a nonconsuming choice for event emission")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , null (tplChoices tpl)
    ]

-- | SPEC-SCALE-003: Unbounded iteration with create, no length check
--
-- Detects functions or choices that iterate over a list parameter
-- (via forA, mapA, traverse, Traversable.forA, etc.) whose callback
-- calls @create@, but the code never asserts an upper bound on the
-- list length.  Without a bound, an attacker can supply an arbitrarily
-- long list and create a proportional number of contracts, consuming
-- resources.
unboundedIterationCreate :: Inspection
unboundedIterationCreate = mkInspection
  "SPEC-SCALE-003"
  "Unbounded iteration with create"
  "A list is iterated (forA/mapA/traverse) with a callback that calls create, but there is no assertion on the list length."
  Warning
  [Scalability]
  $ \mod_ ->
    -- (1) Template choices
    [ mkFinding (InspectionId "SPEC-SCALE-003") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': iterates over '" <> listVar
         <> "' with create in the callback but does not assert a length bound")
        (Just $ "Add 'assertMsg \"too many\" (length " <> listVar <> " <= maxN)' before the iteration")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , listVar <- findUnboundedIterCreate (chBody ch)
    ]
    ++
    -- (2) Standalone functions (DFunction)
    [ mkFinding (InspectionId "SPEC-SCALE-003") Warning loc
        ("Function '" <> fname
         <> "': iterates over '" <> listVar
         <> "' with create in the callback but does not assert a length bound")
        (Just $ "Add 'assertMsg \"too many\" (length " <> listVar <> " <= maxN)' before the iteration")
        Nothing
        Nothing
    | DFunction fname _ body loc <- moduleDecls mod_
    , let stmts = exprToStmtsScale body
    , listVar <- findUnboundedIterCreate stmts
    ]

-- | Extract statements from an expression (handles EDo wrapper)
exprToStmtsScale :: Expr -> [Stmt]
exprToStmtsScale (EDo stmts _) = stmts
exprToStmtsScale _ = []

-- | Iteration functions that process lists element-by-element
iterationFunctions :: [Text]
iterationFunctions =
  [ "forA", "forA_", "mapA", "mapA_"
  , "traverse", "traverse_"
  , "Traversable.forA", "Traversable.forA_"
  , "Traversable.mapA", "Traversable.mapA_"
  , "forM", "forM_", "mapM", "mapM_"
  ]

-- | Find list variables that are iterated but lack bounds checks.
-- Only flags iteration in functions/choices that also perform contract
-- operations (create, exercise, fetch, archive) inside the callback,
-- indicating the iteration involves ledger effects and not just pure computation.
-- Returns the list variable names that are unbounded.
findUnboundedIterCreate :: [Stmt] -> [Text]
findUnboundedIterCreate stmts =
  let -- Find (listVar, callback) pairs from iteration calls
      iterVars = concatMap findIterationListVars stmts
      -- Only keep iterations whose callback contains ledger operations
      ledgerIters = [ listVar | (listVar, callback) <- iterVars
                               , exprHasLedgerOp callback ]
      -- Filter out those that have a preceding length/bounds assertion
      boundedVars = concatMap findBoundsCheckedVars stmts
  in [ v | v <- ledgerIters, v `notElem` boundedVars ]

-- | Check if an expression contains a ledger operation (create, exercise, fetch, archive)
exprHasLedgerOp :: Expr -> Bool
exprHasLedgerOp (EVar v _) = v `elem` ledgerOps
exprHasLedgerOp (EApp f a _) = exprHasLedgerOp f || exprHasLedgerOp a
exprHasLedgerOp (EInfix _ l r _) = exprHasLedgerOp l || exprHasLedgerOp r
exprHasLedgerOp (ELam _ body _) = exprHasLedgerOp body
exprHasLedgerOp (ELet binds body _) = any (exprHasLedgerOp . bindExpr) binds || exprHasLedgerOp body
exprHasLedgerOp (EIf c t e _) = exprHasLedgerOp c || exprHasLedgerOp t || exprHasLedgerOp e
exprHasLedgerOp (EDo stmts _) = any stmtHasLedgerOp stmts
exprHasLedgerOp (EParens e _) = exprHasLedgerOp e
exprHasLedgerOp (ECase e alts _) = exprHasLedgerOp e || any (exprHasLedgerOp . snd) alts
exprHasLedgerOp _ = False

stmtHasLedgerOp :: Stmt -> Bool
stmtHasLedgerOp (SCreate _ _ _) = True
stmtHasLedgerOp (SExercise _ _ _ _) = True
stmtHasLedgerOp (SExerciseByKey _ _ _ _ _) = True
stmtHasLedgerOp (SFetch _ _) = True
stmtHasLedgerOp (SArchive _ _) = True
stmtHasLedgerOp (SBind _ e _) = exprHasLedgerOp e
stmtHasLedgerOp (SExpr e _) = exprHasLedgerOp e
stmtHasLedgerOp (SLet binds _) = any (exprHasLedgerOp . bindExpr) binds
stmtHasLedgerOp (SReturn e _) = exprHasLedgerOp e
stmtHasLedgerOp _ = False

ledgerOps :: [Text]
ledgerOps = ["create", "exercise", "exerciseByKey", "fetch", "fetchByKey", "archive"]

-- | Find (listVarName, callbackExpr) from iteration calls.
-- Matches patterns like: @forA listVar callback@, @Traversable.forA listVar callback@
findIterationListVars :: Stmt -> [(Text, Expr)]
findIterationListVars (SBind _ expr _) = findIterInExpr expr
findIterationListVars (SExpr expr _) = findIterInExpr expr
findIterationListVars (SLet binds _) = concatMap (findIterInExpr . bindExpr) binds
findIterationListVars _ = []

findIterInExpr :: Expr -> [(Text, Expr)]
findIterInExpr (EApp (EApp (EVar fn _) listArg _) callback _)
  | fn `elem` iterationFunctions = [(extractListVarName listArg, callback)]
findIterInExpr (EApp f a _) = findIterInExpr f ++ findIterInExpr a
findIterInExpr (EInfix "<$>" _ b _) = findIterInExpr b
findIterInExpr (EParens e _) = findIterInExpr e
findIterInExpr _ = []

extractListVarName :: Expr -> Text
extractListVarName (EVar v _) = v
extractListVarName (EParens e _) = extractListVarName e
extractListVarName _ = "<list>"

-- | Find variable names that have a bounds/length assertion.
-- Matches patterns like:
--   @assertMsg "..." (length xs <= N)@
--   @assertMsg "..." (List.length xs <= N)@
findBoundsCheckedVars :: Stmt -> [Text]
findBoundsCheckedVars (SAssert _ cond _) = extractBoundsVars cond
findBoundsCheckedVars (SExpr e _) = extractBoundsVarsFromAssert e
findBoundsCheckedVars _ = []

extractBoundsVarsFromAssert :: Expr -> [Text]
extractBoundsVarsFromAssert (EApp (EApp (EVar "assertMsg" _) _ _) cond _) =
  extractBoundsVars cond
extractBoundsVarsFromAssert (EApp f _ _) = extractBoundsVarsFromAssert f
extractBoundsVarsFromAssert _ = []

extractBoundsVars :: Expr -> [Text]
extractBoundsVars (EInfix "<=" (EApp (EVar fn _) (EVar v _) _) _ _)
  | isLengthFunc fn = [v]
extractBoundsVars (EInfix "<" (EApp (EVar fn _) (EVar v _) _) _ _)
  | isLengthFunc fn = [v]
extractBoundsVars (EInfix ">=" _ (EApp (EVar fn _) (EVar v _) _) _)
  | isLengthFunc fn = [v]
extractBoundsVars (EInfix ">" _ (EApp (EVar fn _) (EVar v _) _) _)
  | isLengthFunc fn = [v]
extractBoundsVars (EInfix "&&" l r _) = extractBoundsVars l ++ extractBoundsVars r
extractBoundsVars (EParens e _) = extractBoundsVars e
extractBoundsVars _ = []

isLengthFunc :: Text -> Bool
isLengthFunc f = f `elem` ["length", "List.length", "DA.List.length", "List.size", "Set.size"]
