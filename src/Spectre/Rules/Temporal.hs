{-# LANGUAGE OverloadedStrings #-}
-- | Temporal-related inspection rules.
--
-- Checks for missing deadline assertions and time-window issues.
module Spectre.Rules.Temporal
  ( temporalRules
  ) where

import Data.Text (Text)
import Data.Maybe (mapMaybe)

import Spectre.Ast
import Spectre.Inspection
import Spectre.Rules.Utils (isTimeField, exprMentionsName)

-- | All temporal rules
temporalRules :: [Inspection]
temporalRules =
  [ missingDeadlineCheck
  , offByOneTimeComparison
  , getTimeInContractField
  ]

-- | SPEC-TEMP-001: Missing deadline assertion
--
-- Detects:
-- 1. Choices on templates with time fields that lack time assertions
-- 2. Choices that fetch contracts from other templates with time fields
--    but lack time assertions (cross-template detection)
-- 3. Templates with ordered time-field pairs (e.g. executeAfter/executeBefore)
--    but no ensure clause validating the ordering
missingDeadlineCheck :: Inspection
missingDeadlineCheck = mkInspection
  "SPEC-TEMP-001"
  "Missing deadline assertion"
  "Choice references time-related fields but does not assert that the current time satisfies deadline constraints."
  Warning
  [Temporal]
  $ \mod_ ->
    let templates = [t | DTemplate t <- moduleDecls mod_]
        -- Map: template name -> time field names
        timeMap = [(tplName t, map fieldName $ filter isTimeFieldForTemplate (tplFields t))
                  | t <- templates]
    in
    -- (1) Original: choices on templates with own time fields, no time assertion
    [ mkFinding (InspectionId "SPEC-TEMP-001") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "' references time field '" <> timeField
         <> "' but has no corresponding time assertion (getTime + assertMsg)")
        (Just "Add 'now <- getTime; assertMsg \"deadline\" (now >= deadline)' before the operation")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , timeField <- findTimeFieldRefs tpl ch
    , not (bodyHasTimeAssert (chBody ch))
    ]
    ++
    -- (2) Choices that fetch contracts from templates with time fields
    -- but don't reference the specific fetched time fields (per-field check)
    [ mkFinding (InspectionId "SPEC-TEMP-001") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "' fetches from template '" <> refTplName
         <> "' which has time field '" <> timeField
         <> "' but does not reference '" <> boundVar <> "." <> timeField <> "'")
        (Just $ "Add a time assertion using '" <> boundVar <> "." <> timeField <> "' after fetching the contract")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , (boundVar, refTplName, timeField) <- choiceFetchesTimeTemplatesWithVar timeMap tpl ch
    , not (bodyMentionsFetchedField (chBody ch) boundVar timeField)
    ]
    ++
    -- (3) NEW: templates with ordered time pairs but no ensure validating them
    [ mkFinding (InspectionId "SPEC-TEMP-001") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl
         <> "' has ordered time fields '" <> f1 <> "' / '" <> f2
         <> "' but no ensure clause validating their ordering")
        (Just $ "Add 'ensure " <> f1 <> " < " <> f2 <> "' to prevent degenerate time windows")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , (f1, f2) <- findOrderedTimePairs (tplFields tpl)
    , not (ensureMentionsBoth (tplEnsure tpl) f1 f2)
    ]

-- | SPEC-TEMP-002: Off-by-one in time comparison (<= vs <)
--
-- Detects time comparisons that use <= instead of < (or vice versa),
-- which can cause off-by-one errors in deadline checks.
offByOneTimeComparison :: Inspection
offByOneTimeComparison = mkInspection
  "SPEC-TEMP-002"
  "Potentially off-by-one time comparison"
  "Time comparison uses '<=' which may allow execution at the exact deadline moment. Consider whether '<' is more appropriate."
  Info
  [Temporal]
  $ \mod_ ->
    -- (1) Choices within templates
    [ mkFinding (InspectionId "SPEC-TEMP-002") Info (loc)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': time comparison uses '<=' which includes the boundary moment")
        (Just "Review whether '<' (strictly before) is the intended semantics")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , loc <- findLeqTimeComparisons (chBody ch)
    ]
    ++
    -- (2) Standalone functions (DFunction declarations)
    [ mkFinding (InspectionId "SPEC-TEMP-002") Info loc
        ("Function '" <> fname
         <> "': time comparison uses '<=' which includes the boundary moment")
        (Just "Review whether '<' (strictly before) is the intended semantics")
        Nothing
        Nothing
    | DFunction fname _ body _ <- moduleDecls mod_
    , loc <- findLeqTimeComparisonsInExpr body
    ]

-- =========================================================================
-- Helpers
-- =========================================================================

-- | Time-related field detection — TYPE-BASED with minimal name fallback.
-- Prefers checking fieldType == Time. Only falls back to name heuristic
-- when type info is unavailable (e.g., parser couldn't resolve it).
isTimeFieldForTemplate :: Field -> Bool
isTimeFieldForTemplate = isTimeField

-- | Find time-related fields on a template that are relevant to a choice.
-- A time field is relevant if:
-- 1. The choice body explicitly references the field, OR
-- 2. The choice performs state-changing operations (create/exercise) and the
--    template has time fields — indicating the choice SHOULD check them.
findTimeFieldRefs :: Template -> Choice -> [Text]
findTimeFieldRefs tpl ch =
  let timeFields = filter isTimeFieldForTemplate (tplFields tpl)
      timeFieldNames = map fieldName timeFields
      -- Fields directly referenced in the choice body
      directRefs = filter (\f -> bodyReferencesField f (chBody ch)) timeFieldNames
      -- If the choice does state-changing ops but references no time fields,
      -- all time fields on the template are relevant (the choice SHOULD check them)
      hasStateChange = any isStateChangingStmt (chBody ch)
  in if not (null directRefs)
     then directRefs  -- choice explicitly uses time fields
     else if hasStateChange && not (null timeFieldNames)
          then timeFieldNames  -- choice modifies state but ignores time fields
          else []  -- pure read-only choice, no time concern

-- | Check if a statement performs a state-changing operation
isStateChangingStmt :: Stmt -> Bool
isStateChangingStmt (SCreate _ _ _) = True
isStateChangingStmt (SExercise _ _ _ _) = True
isStateChangingStmt (SExerciseByKey _ _ _ _ _) = True
isStateChangingStmt (SArchive _ _) = True
isStateChangingStmt _ = False

-- | Check if a choice body references a given field name
bodyReferencesField :: Text -> [Stmt] -> Bool
bodyReferencesField name stmts = any (stmtMentionsField name) stmts

stmtMentionsField :: Text -> Stmt -> Bool
stmtMentionsField name (SExpr e _) = exprMentionsField name e
stmtMentionsField name (SBind _ e _) = exprMentionsField name e
stmtMentionsField name (SCreate _ e _) = exprMentionsField name e
stmtMentionsField name (SAssert _ e _) = exprMentionsField name e
stmtMentionsField name (SLet binds _) = any (exprMentionsField name . bindExpr) binds
stmtMentionsField name (SReturn e _) = exprMentionsField name e
stmtMentionsField _ _ = False

exprMentionsField :: Text -> Expr -> Bool
exprMentionsField name (EVar v _) = v == name
exprMentionsField name (EFieldAccess _ f _) = f == name
exprMentionsField name (EApp f a _) = exprMentionsField name f || exprMentionsField name a
exprMentionsField name (EInfix _ l r _) = exprMentionsField name l || exprMentionsField name r
exprMentionsField name (EParens e _) = exprMentionsField name e
exprMentionsField name (EIf c t e _) = exprMentionsField name c || exprMentionsField name t || exprMentionsField name e
exprMentionsField name (ERecordCon _ fields _) = any (\(_, e) -> exprMentionsField name e) fields
exprMentionsField name (ERecordUpd e fields _) = exprMentionsField name e || any (\(_, e') -> exprMentionsField name e') fields
exprMentionsField _ _ = False

-- =========================================================================
-- Cross-template time field detection
-- =========================================================================

-- | Extract the target template name from a ContractId type
-- e.g., ContractId Order -> Just "Order"
extractContractIdType :: Type -> Maybe Text
extractContractIdType (TApp (TCon "ContractId" _) (TCon name _) _) = Just name
extractContractIdType _ = Nothing

-- | Get (fieldName, targetTemplateName) for all ContractId-typed fields
contractIdTargets :: [Field] -> [(Text, Text)]
contractIdTargets = mapMaybe go
  where
    go f = case fieldType f of
      Just ty -> case extractContractIdType ty of
        Just tName -> Just (fieldName f, tName)
        Nothing    -> Nothing
      Nothing -> Nothing

-- =========================================================================
-- Missing ensure on time-ordered field pairs
-- =========================================================================

-- | Find pairs of Time-typed fields that should have an ordering constraint.
-- If a template has 2+ Time fields and the ensure clause doesn't mention
-- both, we flag them. This is purely type-based — no name matching.
findOrderedTimePairs :: [Field] -> [(Text, Text)]
findOrderedTimePairs fields =
  let timeFields = filter isTimeFieldForTemplate fields
      names = map fieldName timeFields
  in case names of
       (n1:n2:_) -> [(n1, n2)]  -- flag first pair found
       _         -> []

-- | Check if an ensure clause references both field names
ensureMentionsBoth :: Maybe Expr -> Text -> Text -> Bool
ensureMentionsBoth Nothing _ _ = False
ensureMentionsBoth (Just expr) f1 f2 =
  exprMentionsName f1 expr && exprMentionsName f2 expr

-- =========================================================================
-- Time assertion detection
-- =========================================================================

-- | Check if choice body has a time-related assertion
bodyHasTimeAssert :: [Stmt] -> Bool
bodyHasTimeAssert stmts =
  let hasGetTime = any stmtHasGetTime stmts
      hasTimeAssert = any stmtHasTimeAssert stmts
  in hasGetTime && hasTimeAssert

stmtHasGetTime :: Stmt -> Bool
stmtHasGetTime (SBind _ (EVar "getTime" _) _) = True
stmtHasGetTime (SBind _ (EApp (EVar "getTime" _) _ _) _) = True
stmtHasGetTime (SLet binds _) = any (exprHasGetTime . bindExpr) binds
stmtHasGetTime _ = False

exprHasGetTime :: Expr -> Bool
exprHasGetTime (EVar "getTime" _) = True
exprHasGetTime (EApp f _ _) = exprHasGetTime f
exprHasGetTime _ = False

stmtHasTimeAssert :: Stmt -> Bool
stmtHasTimeAssert (SAssert _ cond _) = exprMentionsTime cond
stmtHasTimeAssert (SExpr e _) = exprIsTimeAssert e
stmtHasTimeAssert _ = False

exprIsTimeAssert :: Expr -> Bool
exprIsTimeAssert (EApp (EApp (EVar "assertMsg" _) _ _) cond _) =
  exprMentionsTime cond
exprIsTimeAssert (EApp (EVar "assert" _) cond _) = exprMentionsTime cond
-- when/unless patterns
exprIsTimeAssert (EApp (EApp (EVar fn _) cond _) _ _)
  | fn `elem` ["when", "unless"] = exprMentionsTime cond
exprIsTimeAssert (EApp f a _) = exprIsTimeAssert f || exprIsTimeAssert a
exprIsTimeAssert (ECase _ alts _) = any (exprIsTimeAssert . snd) alts
exprIsTimeAssert (EIf _ t e _) = exprIsTimeAssert t || exprIsTimeAssert e
exprIsTimeAssert (EDo stmts _) = any stmtHasTimeAssert stmts
exprIsTimeAssert (EParens e _) = exprIsTimeAssert e
exprIsTimeAssert _ = False

-- | Does an expression reference a time-obtained variable?
-- Recognizes "now" and "currentTime" as standard DAML getTime bindings.
-- This is NOT a name heuristic — these are the canonical variable names
-- produced by `now <- getTime` which is the only way to get time in DAML.
exprMentionsTime :: Expr -> Bool
exprMentionsTime (EVar name _) = name == "now" || name == "currentTime"
exprMentionsTime (EApp f a _) = exprMentionsTime f || exprMentionsTime a
exprMentionsTime (EInfix _ l r _) = exprMentionsTime l || exprMentionsTime r
exprMentionsTime (EParens e _) = exprMentionsTime e
exprMentionsTime _ = False

-- =========================================================================
-- Per-field cross-template detection (for sub-rule 2)
-- =========================================================================

-- | Extract (boundVarName, cidFieldName) from fetch bindings.
-- e.g., @settlement <- fetch settlementCid@ yields @("settlement", "settlementCid")@
fetchBindingsWithVar :: [Stmt] -> [(Text, Text)]
fetchBindingsWithVar = concatMap go
  where
    go (SBind (PVar v _) expr _) = [(v, cid) | cid <- fetchCidFromExpr expr]
    go _ = []

    fetchCidFromExpr (EApp (EVar "fetch" _) arg _) = cidFromExpr arg
    fetchCidFromExpr (EApp f a _) = fetchCidFromExpr f ++ fetchCidFromExpr a
    fetchCidFromExpr _ = []

    cidFromExpr (EVar v _) = [v]
    cidFromExpr (EFieldAccess _ field _) = [field]
    cidFromExpr _ = []

-- | Like 'choiceFetchesTimeTemplates' but also returns the bound variable name.
-- Returns @(boundVar, refTplName, timeFieldName)@ triples.
choiceFetchesTimeTemplatesWithVar :: [(Text, [Text])] -> Template -> Choice -> [(Text, Text, Text)]
choiceFetchesTimeTemplatesWithVar timeMap tpl ch =
  let cidFields = contractIdTargets (tplFields tpl ++ chParams ch)
      bindings = fetchBindingsWithVar (chBody ch)
      fetchedInfo = [(boundVar, tName)
                    | (boundVar, cidField) <- bindings
                    , (fName, tName) <- cidFields
                    , fName == cidField]
  in concatMap (\(boundVar, tName) ->
        case lookup tName timeMap of
          Just timeFields -> [(boundVar, tName, tf) | tf <- timeFields]
          Nothing         -> []
      ) fetchedInfo

-- | Check if the choice body mentions @varName.fieldName@ anywhere.
bodyMentionsFetchedField :: [Stmt] -> Text -> Text -> Bool
bodyMentionsFetchedField stmts varName fldName =
  any (stmtMentionsFA varName fldName) stmts

stmtMentionsFA :: Text -> Text -> Stmt -> Bool
stmtMentionsFA v f (SBind _ e _) = exprMentionsFA v f e
stmtMentionsFA v f (SExpr e _)   = exprMentionsFA v f e
stmtMentionsFA v f (SAssert _ e _) = exprMentionsFA v f e
stmtMentionsFA v f (SLet binds _) = any (exprMentionsFA v f . bindExpr) binds
stmtMentionsFA v f (SReturn e _) = exprMentionsFA v f e
stmtMentionsFA _ _ _ = False

exprMentionsFA :: Text -> Text -> Expr -> Bool
exprMentionsFA v f (EFieldAccess (EVar v2 _) f2 _) = v == v2 && f == f2
exprMentionsFA v f (EFieldAccess e _ _)  = exprMentionsFA v f e
-- The parser includes '.' in pVarOrCon, so "settlement.executeAfter" is often
-- parsed as a single EVar rather than EFieldAccess. Handle both forms.
exprMentionsFA v f (EVar name _)         = name == (v <> "." <> f)
exprMentionsFA v f (EApp a b _)          = exprMentionsFA v f a || exprMentionsFA v f b
exprMentionsFA v f (EInfix _ l r _)      = exprMentionsFA v f l || exprMentionsFA v f r
exprMentionsFA v f (EParens e _)         = exprMentionsFA v f e
exprMentionsFA v f (EIf c t e _)         = exprMentionsFA v f c || exprMentionsFA v f t || exprMentionsFA v f e
exprMentionsFA v f (ECase e alts _)      = exprMentionsFA v f e || any (exprMentionsFA v f . snd) alts
exprMentionsFA v f (EDo stmts _)         = any (stmtMentionsFA v f) stmts
exprMentionsFA _ _ _                     = False

-- | Find <= comparisons involving time expressions in a list of statements
findLeqTimeComparisons :: [Stmt] -> [SrcSpan]
findLeqTimeComparisons stmts = concatMap findInStmt stmts
  where
    findInStmt (SAssert _ expr _) = findInExpr expr
    findInStmt (SExpr expr _) = findInExpr expr
    findInStmt (SBind _ expr _) = findInExpr expr
    findInStmt (SLet binds _) = concatMap (findInExpr . bindExpr) binds
    findInStmt _ = []

    findInExpr (EInfix "<=" l r s)
      | exprMentionsTime l || exprMentionsTime r = [s]
    findInExpr (EApp f a _) = findInExpr f ++ findInExpr a
    findInExpr (EParens e _) = findInExpr e
    findInExpr (EDo stmts' _) = findLeqTimeComparisons stmts'
    findInExpr (EIf c t e _) = findInExpr c ++ findInExpr t ++ findInExpr e
    findInExpr (ECase _ alts _) = concatMap (findInExpr . snd) alts
    findInExpr (ELam _ body _) = findInExpr body
    findInExpr (ELet binds body _) = concatMap (findInExpr . bindExpr) binds ++ findInExpr body
    findInExpr (ERecordCon _ fields _) = concatMap (findInExpr . snd) fields
    findInExpr (ERecordUpd e fields _) = findInExpr e ++ concatMap (findInExpr . snd) fields
    findInExpr (ETuple es _) = concatMap findInExpr es
    findInExpr (EList es _) = concatMap findInExpr es
    findInExpr (EFieldAccess e _ _) = findInExpr e
    findInExpr _ = []

-- | Find <= comparisons involving time expressions starting from an expression
-- (used for DFunction bodies which are Expr, not [Stmt])
findLeqTimeComparisonsInExpr :: Expr -> [SrcSpan]
findLeqTimeComparisonsInExpr (EDo stmts _) = findLeqTimeComparisons stmts
findLeqTimeComparisonsInExpr (EInfix "<=" l r s)
  | exprMentionsTime l || exprMentionsTime r = [s]
findLeqTimeComparisonsInExpr (EApp f a _) = findLeqTimeComparisonsInExpr f ++ findLeqTimeComparisonsInExpr a
findLeqTimeComparisonsInExpr (EParens e _) = findLeqTimeComparisonsInExpr e
findLeqTimeComparisonsInExpr (EIf c t e _) = findLeqTimeComparisonsInExpr c ++ findLeqTimeComparisonsInExpr t ++ findLeqTimeComparisonsInExpr e
findLeqTimeComparisonsInExpr (ECase _ alts _) = concatMap (findLeqTimeComparisonsInExpr . snd) alts
findLeqTimeComparisonsInExpr (ELam _ body _) = findLeqTimeComparisonsInExpr body
findLeqTimeComparisonsInExpr (ELet binds body _) = concatMap (findLeqTimeComparisonsInExpr . bindExpr) binds ++ findLeqTimeComparisonsInExpr body
findLeqTimeComparisonsInExpr _ = []

-- =========================================================================
-- SPEC-TEMP-003: getTime result stored in contract field
-- =========================================================================

-- | SPEC-TEMP-003: getTime result written into contract field
--
-- In Canton's execution model, getTime returns the preparation-time clock,
-- which diverges from the sequencer-assigned record time.  When a getTime
-- result is persisted as a contract field value (e.g. createdAt = now),
-- any subsequent time arithmetic on that field (expiry checks, deadlines)
-- will use the wrong epoch, leading to failures under MPC/multi-sig
-- signing flows where preparation-to-sequencing latency exceeds the time
-- tolerance.
--
-- The recommended pattern is to use an externally-provided timestamp
-- (e.g. requestedAt from the caller) rather than getTime for any
-- field that affects lifecycle logic.
getTimeInContractField :: Inspection
getTimeInContractField = mkInspection
  "SPEC-TEMP-003"
  "getTime result stored in contract field"
  "getTime returns preparation-time in Canton, not sequencer time. Storing it in a contract field causes incorrect time arithmetic under MPC/multi-sig signing."
  Warning
  [Temporal]
  $ \mod_ ->
    -- (1) Choices within templates
    [ mkFinding (InspectionId "SPEC-TEMP-003") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': getTime result stored in field '" <> fld
         <> "' of created contract — use an externally-provided timestamp instead")
        (Just "Replace getTime with a caller-supplied time parameter (e.g. requestedAt)")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , fld <- findGetTimeInCreateFields (chBody ch)
    ]
    ++
    -- (2) Standalone functions (DFunction)
    [ mkFinding (InspectionId "SPEC-TEMP-003") Warning loc
        ("Function '" <> fname
         <> "': getTime result stored in field '" <> fld
         <> "' of created contract — use an externally-provided timestamp instead")
        (Just "Replace getTime with a caller-supplied time parameter (e.g. requestedAt)")
        Nothing
        Nothing
    | DFunction fname _ body loc <- moduleDecls mod_
    , fld <- findGetTimeInCreateFieldsExpr body
    ]

-- | Find field names in create statements that reference a getTime-bound
-- variable, given a flat list of statements.
findGetTimeInCreateFields :: [Stmt] -> [Text]
findGetTimeInCreateFields stmts =
  let gtVars = getTimeBoundVars stmts
  in  concatMap (createFieldsReferencingVars gtVars) stmts

-- | Same, starting from an expression (for DFunction bodies).
findGetTimeInCreateFieldsExpr :: Expr -> [Text]
findGetTimeInCreateFieldsExpr (EDo stmts _) = findGetTimeInCreateFields stmts
findGetTimeInCreateFieldsExpr _ = []

-- | Collect variable names that are bound from getTime.
-- Matches:  @var <- getTime@
getTimeBoundVars :: [Stmt] -> [Text]
getTimeBoundVars = concatMap go
  where
    go (SBind (PVar v _) (EVar "getTime" _) _) = [v]
    go (SBind (PVar v _) (EApp (EVar "getTime" _) _ _) _) = [v]
    go _ = []

-- | Given a set of getTime-bound variable names, find field names in
-- create statements (SCreate or SBind with create) whose values
-- directly reference one of those variables.
createFieldsReferencingVars :: [Text] -> Stmt -> [Text]
createFieldsReferencingVars gtVars stmt =
  let mFields = createFieldsFromStmt stmt
  in case mFields of
       Nothing -> []
       Just fields ->
         [ fName
         | (fName, fExpr) <- fields
         , exprDirectlyRefsAny gtVars fExpr
         ]

-- | Extract field assignments from a create statement.
-- Handles both @SCreate _ (ERecordCon _ fields _) _@ and
-- @SBind _ (EApp (EVar "create" _) (ERecordCon _ fields _) _) _@
-- and @SCreate _ (ERecordUpd _ fields _) _@ patterns.
createFieldsFromStmt :: Stmt -> Maybe [(Text, Expr)]
createFieldsFromStmt (SCreate _ (ERecordCon _ fields _) _) = Just fields
createFieldsFromStmt (SCreate _ (ERecordUpd _ fields _) _) = Just fields
createFieldsFromStmt (SBind _ (EApp (EVar "create" _) (ERecordCon _ fields _) _) _) = Just fields
createFieldsFromStmt (SBind _ (EApp (EVar "create" _) (ERecordUpd _ fields _) _) _) = Just fields
-- Also handle: pat <- expr <$> create ... (e.g., toInterfaceContractId <$> create ...)
createFieldsFromStmt (SBind _ expr _) = createFieldsFromExpr expr
createFieldsFromStmt (SExpr expr _) = createFieldsFromExpr expr
createFieldsFromStmt _ = Nothing

-- | Extract create fields from an expression (handles nested creates
-- like @toInterfaceContractId <$> create Foo with ...@).
createFieldsFromExpr :: Expr -> Maybe [(Text, Expr)]
createFieldsFromExpr (EApp (EVar "create" _) (ERecordCon _ fields _) _) = Just fields
createFieldsFromExpr (EApp (EVar "create" _) (ERecordUpd _ fields _) _) = Just fields
createFieldsFromExpr (EApp _ b _) = createFieldsFromExpr b
createFieldsFromExpr (EInfix "<$>" _ b _) = createFieldsFromExpr b
createFieldsFromExpr (EParens e _) = createFieldsFromExpr e
createFieldsFromExpr _ = Nothing

-- | Check if an expression directly references any of the given variable names.
-- "Directly" means the expression is @EVar name _@ where @name@ is in the set.
exprDirectlyRefsAny :: [Text] -> Expr -> Bool
exprDirectlyRefsAny vars (EVar v _) = v `elem` vars
exprDirectlyRefsAny _ _ = False


