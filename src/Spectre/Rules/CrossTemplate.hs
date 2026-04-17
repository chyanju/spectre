{-# LANGUAGE OverloadedStrings #-}
-- | Cross-template inspection rules.
--
-- Checks for cross-template issues and diagnostic problems.
module Spectre.Rules.CrossTemplate
  ( crossTemplateRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Spectre.Ast
import Spectre.Inspection

-- | All cross-template rules
crossTemplateRules :: [Inspection]
crossTemplateRules =
  [ duplicateErrorMessages
  , duplicateEventDiscriminators
  , misleadingAssertMessage
  , deadCodeUnusedDefinition
  , misleadingChoiceName
  , cancelWithoutAuditTrail
  ]

-- | SPEC-DIAG-001: Duplicate or indistinguishable error messages
--
-- Detects assertMsg calls with identical or very similar messages,
-- which makes debugging difficult when an assertion fails.
duplicateErrorMessages :: Inspection
duplicateErrorMessages = mkInspection
  "SPEC-DIAG-001"
  "Duplicate error messages"
  "Multiple assertMsg calls use identical error messages, making it difficult to identify which assertion failed."
  Info
  [Diagnostics]
  $ \mod_ ->
    let allAsserts = collectAssertMessages mod_
        duplicates = Map.filter (\locs -> length locs > 1) allAsserts
    in [ mkFinding (InspectionId "SPEC-DIAG-001") Info loc
          ("Duplicate assertMsg message: \"" <> msg <> "\" (appears " <> T.pack (show (length locs)) <> " times)")
          (Just "Use unique, descriptive error messages for each assertion")
          tpl ch
       | (msg, locs) <- Map.toList duplicates
       , (loc, tpl, ch) <- locs
       ]

-- Helpers

type AssertInfo = (SrcSpan, Maybe Text, Maybe Text)  -- location, template, choice

collectAssertMessages :: Module -> Map Text [AssertInfo]
collectAssertMessages mod_ = Map.fromListWith (++)
  [ (msg, [(loc, Just (tplName tpl), Just (chName ch))])
  | DTemplate tpl <- moduleDecls mod_
  , ch <- tplChoices tpl
  , (msg, loc) <- findAssertMsgs (chBody ch)
  ]

findAssertMsgs :: [Stmt] -> [(Text, SrcSpan)]
findAssertMsgs = concatMap findInStmt
  where
    findInStmt (SAssert msg _ s)
      | not (T.null msg) = [(msg, s)]
    findInStmt (SExpr e _) = findInExpr e
    findInStmt (SBind _ e _) = findInExpr e
    findInStmt (SLet binds _) = concatMap (findInExpr . bindExpr) binds
    findInStmt _ = []

    findInExpr (EApp (EApp (EVar "assertMsg" _) (EString msg _) _) _ s) = [(msg, s)]
    findInExpr (EApp f a _) = findInExpr f ++ findInExpr a
    findInExpr (ECase _ alts _) = concatMap (findInExpr . snd) alts
    findInExpr (EIf _ t e _) = findInExpr t ++ findInExpr e
    findInExpr (EDo stmts _) = concatMap findInStmt stmts
    findInExpr (EParens e _) = findInExpr e
    findInExpr _ = []

-- | SPEC-DIAG-002: Duplicate event discriminator literals
--
-- Detects sibling choices (within the same template) that create
-- the same event/record type with the same discriminator literal
-- (typically an action enum value), making audit events
-- indistinguishable across different operations.
duplicateEventDiscriminators :: Inspection
duplicateEventDiscriminators = mkInspection
  "SPEC-DIAG-002"
  "Duplicate event discriminator"
  "Sibling choices create the same event type with the same action/discriminator literal, making audit events indistinguishable."
  Warning
  [Diagnostics]
  $ \mod_ ->
    let templates = [tpl | DTemplate tpl <- moduleDecls mod_]
    in concatMap checkDupEvents templates
    ++
    -- Also check top-level functions: if two sibling functions create
    -- events with identical discriminators
    let funcs = [(n, body, loc) | DFunction n _ body loc <- moduleDecls mod_]
    in checkFuncDupEvents funcs

checkDupEvents :: Template -> [Finding]
checkDupEvents tpl =
  let choices = tplChoices tpl
      -- For each choice, extract (choiceName, [(eventType, discriminatorLiteral)])
      choiceEvents = [(chName ch, findEventCreations (chBody ch)) | ch <- choices]
      -- Find duplicates: different choices creating same (type, discriminator) pair
  in [ mkFinding (InspectionId "SPEC-DIAG-002") Warning (chLocation ch2)
        ("Template '" <> tplName tpl <> "': choices '" <> cn1
         <> "' and '" <> cn2 <> "' both create '" <> evType
         <> "' with discriminator '" <> disc
         <> "' — audit events will be indistinguishable")
        (Just $ "Use a distinct discriminator for choice '" <> cn2 <> "'")
        (Just (tplName tpl))
        (Just cn2)
     | (cn1, evs1) <- choiceEvents
     , (cn2, evs2) <- choiceEvents
     , cn1 < cn2  -- avoid duplicates
     , (evType, disc) <- evs1
     , (evType2, disc2) <- evs2
     , evType == evType2 && disc == disc2
     , ch2 <- [ch | ch <- tplChoices tpl, chName ch == cn2]
     ]

-- | Check for duplicate discriminators across top-level functions
checkFuncDupEvents :: [(Text, Expr, SrcSpan)] -> [Finding]
checkFuncDupEvents funcs =
  let funcEvents = [(n, findEventCreationsInExpr body, loc) | (n, body, loc) <- funcs]
  in [ mkFinding (InspectionId "SPEC-DIAG-002") Warning loc2
        ("Functions '" <> fn1 <> "' and '" <> fn2
         <> "' both create '" <> evType <> "' with discriminator '" <> disc
         <> "' — audit events will be indistinguishable")
        (Just $ "Use a distinct discriminator for function '" <> fn2 <> "'")
        Nothing
        Nothing
     | (fn1, evs1, _loc1) <- funcEvents
     , (fn2, evs2, loc2) <- funcEvents
     , fn1 < fn2
     , (evType, disc) <- evs1
     , (evType2, disc2) <- evs2
     , evType == evType2 && disc == disc2
     ]

-- | Find event creation patterns in statements: create EventType with { ...action = Discriminator... }
-- Returns (eventTypeName, discriminatorLiteral) pairs
findEventCreations :: [Stmt] -> [(Text, Text)]
findEventCreations = concatMap findInStmt
  where
    findInStmt (SCreate (TCon typeName _) body _) = extractDiscriminators typeName body
    findInStmt (SBind _ e _) = findEventCreationsInExpr e
    findInStmt (SExpr e _) = findEventCreationsInExpr e
    findInStmt _ = []

-- | Find event creations in an expression tree
findEventCreationsInExpr :: Expr -> [(Text, Text)]
findEventCreationsInExpr expr = case expr of
  EApp (EVar "create" _) (ERecordCon typeName fields _) _ ->
    extractDiscriminatorsFromFields typeName fields
  EApp f a _ -> findEventCreationsInExpr f ++ findEventCreationsInExpr a
  EParens e _ -> findEventCreationsInExpr e
  EIf _ t e _ -> findEventCreationsInExpr t ++ findEventCreationsInExpr e
  ECase _ alts _ -> concatMap (findEventCreationsInExpr . snd) alts
  EDo stmts _ -> findEventCreations stmts
  _ -> []

-- | Extract discriminator literals from a create expression body
extractDiscriminators :: Text -> Expr -> [(Text, Text)]
extractDiscriminators typeName (ERecordCon _ fields _) = extractDiscriminatorsFromFields typeName fields
extractDiscriminators typeName (ERecordUpd _ fields _) = extractDiscriminatorsFromFields typeName fields
extractDiscriminators _ _ = []

extractDiscriminatorsFromFields :: Text -> [(Text, Expr)] -> [(Text, Text)]
extractDiscriminatorsFromFields typeName fields =
  -- Extract ALL string/enum literal field assignments as potential discriminators.
  -- No name-based filtering — any literal field could be a discriminator.
  [ (typeName, litVal)
  | (_fName, fExpr) <- fields
  , litVal <- extractLiteral fExpr
  ]

extractLiteral :: Expr -> [Text]
extractLiteral (EVar name _) = [name]  -- enum constructors like AdminBurnNative
extractLiteral (EString s _) = [s]
extractLiteral (EParens e _) = extractLiteral e
extractLiteral _ = []

-- | SPEC-DIAG-003: Misleading assertMsg message
--
-- Detects assertMsg calls where the error message describes the
-- *success* condition rather than the *failure* condition.  When the
-- assertion fails, the operator sees a message that says what SHOULD be
-- true, not what went wrong.
-- Heuristic: if the message contains words like "matches", "equals",
-- "is valid", "is correct" without negation words, it's likely
-- describing the success condition.
misleadingAssertMessage :: Inspection
misleadingAssertMessage = mkInspection
  "SPEC-DIAG-003"
  "Misleading assertMsg message"
  "An assertMsg error message describes the success condition rather than the failure. When the assertion fails, operators see a misleading message."
  Info
  [Diagnostics]
  $ \mod_ ->
    -- (1) In template choices
    [ mkFinding (InspectionId "SPEC-DIAG-003") Info loc
        ("Template '" <> tplName tpl <> "', choice '" <> chName ch
         <> "': assertMsg message \"" <> msg
         <> "\" describes success condition — should describe the failure")
        (Just "Reword the message to describe what went wrong, e.g., 'expected admin does not match actual admin'")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , (msg, loc) <- findMisleadingAsserts (chBody ch)
    ]
    ++
    -- (2) In top-level functions
    [ mkFinding (InspectionId "SPEC-DIAG-003") Info loc
        ("Function '" <> name <> "': assertMsg message \""
         <> msg <> "\" describes success condition — should describe the failure")
        (Just "Reword the message to describe what went wrong")
        Nothing
        Nothing
    | DFunction name _ body _loc <- moduleDecls mod_
    , (msg, loc) <- findMisleadingAssertsInExpr body
    ]

findMisleadingAsserts :: [Stmt] -> [(Text, SrcSpan)]
findMisleadingAsserts = concatMap check
  where
    check (SAssert msg _ s)
      | isMisleadingMsg msg = [(msg, s)]
    check (SExpr e _) = findMisleadingAssertsInExpr e
    check (SBind _ e _) = findMisleadingAssertsInExpr e
    check _ = []

findMisleadingAssertsInExpr :: Expr -> [(Text, SrcSpan)]
findMisleadingAssertsInExpr (EApp (EApp (EVar "assertMsg" _) msgExpr _) _ s)
  | let msg = extractStaticStringParts msgExpr
  , not (T.null msg)
  , isMisleadingMsg msg = [(msg, s)]
findMisleadingAssertsInExpr (EApp f a _) =
  findMisleadingAssertsInExpr f ++ findMisleadingAssertsInExpr a
findMisleadingAssertsInExpr (ECase _ alts _) =
  concatMap (findMisleadingAssertsInExpr . snd) alts
findMisleadingAssertsInExpr (EIf _ t e _) =
  findMisleadingAssertsInExpr t ++ findMisleadingAssertsInExpr e
findMisleadingAssertsInExpr (EDo stmts _) = findMisleadingAsserts stmts
findMisleadingAssertsInExpr (EParens e _) = findMisleadingAssertsInExpr e
findMisleadingAssertsInExpr _ = []

-- | Extract static string content from an expression that may be a string
-- literal or a concatenation of literals and dynamic parts.
-- E.g., ("Expected " <> show x <> " matches " <> show y) → "Expected  matches "
extractStaticStringParts :: Expr -> Text
extractStaticStringParts (EString s _) = s
extractStaticStringParts (EParens e _) = extractStaticStringParts e
extractStaticStringParts (EInfix "<>" l r _) =
  extractStaticStringParts l <> extractStaticStringParts r
extractStaticStringParts (EInfix "++" l r _) =
  extractStaticStringParts l <> extractStaticStringParts r
extractStaticStringParts _ = ""  -- dynamic part (show x, variable, etc.)

-- | Heuristic: a message is "misleading" if it describes a positive/success condition
-- without any negation. E.g., "Expected admin matches actual admin" describes what SHOULD
-- be true, but the assertion message should describe the ERROR condition.
isMisleadingMsg :: Text -> Bool
isMisleadingMsg msg =
  let lower = T.toLower msg
      hasSuccessWord = any (`T.isInfixOf` lower)
        ["matches", "equals", "is valid", "is correct", "is authorized", "is allowed", "has permission"]
      hasNegationWord = any (`T.isInfixOf` lower)
        ["not", "no ", "missing", "invalid", "incorrect", "denied", "forbidden", "must", "failed", "error", "mismatch", "rejected"]
  in hasSuccessWord && not hasNegationWord

-- | SPEC-DIAG-004: Dead code / unused top-level definition
--
-- Detects top-level function definitions that are not referenced by
-- any other declaration in the same module.  Dead code increases
-- maintenance burden and review overhead.
deadCodeUnusedDefinition :: Inspection
deadCodeUnusedDefinition = mkInspection
  "SPEC-DIAG-004"
  "Unused top-level definition"
  "A top-level function or value is defined but not referenced elsewhere in the module. Dead code increases maintenance burden."
  Info
  [Diagnostics]
  $ \mod_ ->
    let decls = moduleDecls mod_
        -- Collect all top-level function names
        funcNames = Set.fromList [name | DFunction name _ _ _ <- decls]
        -- Collect all names referenced in the bodies of all decls
        allRefs = collectAllReferences decls
        -- Collect field names from data type declarations — these are
        -- auto-generated record accessors and should not be flagged.
        dataFieldNames = Set.fromList
          [fieldName f | DDataType _ fields _ <- decls, f <- fields]
        -- Find unused: defined but never referenced, excluding data-derived accessors
        unused = Set.difference funcNames (Set.union allRefs dataFieldNames)
    in [ mkFinding (InspectionId "SPEC-DIAG-004") Info loc
          ("Unused top-level definition '" <> name
           <> "' — not referenced by any other declaration in this module")
          (Just $ "Remove '" <> name <> "' or export it if it's used externally")
          Nothing
          Nothing
       | DFunction name _ _ loc <- decls
       , name `Set.member` unused
       , not ("main" `T.isPrefixOf` T.toLower name)   -- don't flag main
       , not ("_" `T.isPrefixOf` name)                 -- don't flag _-prefixed
       ]

-- | Collect all variable references from all declarations
collectAllReferences :: [Decl] -> Set Text
collectAllReferences decls = Set.fromList $ concatMap collectFromDecl decls
  where
    collectFromDecl (DTemplate tpl) =
      concatMap (concatMap collectFromStmt . chBody) (tplChoices tpl)
      ++ maybe [] collectFromExpr (tplEnsure tpl)
    collectFromDecl (DFunction _ _ body _) = collectFromExpr body
    collectFromDecl _ = []

    collectFromStmt (SExpr e _) = collectFromExpr e
    collectFromStmt (SBind _ e _) = collectFromExpr e
    collectFromStmt (SCreate _ e _) = collectFromExpr e
    collectFromStmt (SExercise e _ a _) = collectFromExpr e ++ collectFromExpr a
    collectFromStmt (SFetch e _) = collectFromExpr e
    collectFromStmt (SArchive e _) = collectFromExpr e
    collectFromStmt (SAssert _ e _) = collectFromExpr e
    collectFromStmt (SReturn e _) = collectFromExpr e
    collectFromStmt (SLet binds _) = concatMap (collectFromExpr . bindExpr) binds
    collectFromStmt _ = []

    collectFromExpr (EVar name _) = [name]
    collectFromExpr (EApp f a _) = collectFromExpr f ++ collectFromExpr a
    collectFromExpr (EInfix _ l r _) = collectFromExpr l ++ collectFromExpr r
    collectFromExpr (EFieldAccess e f _) = f : collectFromExpr e
    collectFromExpr (EParens e _) = collectFromExpr e
    collectFromExpr (EIf c t e _) = collectFromExpr c ++ collectFromExpr t ++ collectFromExpr e
    collectFromExpr (ELet binds body _) = concatMap (collectFromExpr . bindExpr) binds ++ collectFromExpr body
    collectFromExpr (ECase scrut alts _) = collectFromExpr scrut ++ concatMap (collectFromExpr . snd) alts
    collectFromExpr (EDo stmts _) = concatMap collectFromStmt stmts
    collectFromExpr (ELam _ body _) = collectFromExpr body
    collectFromExpr (EList es _) = concatMap collectFromExpr es
    collectFromExpr (ETuple es _) = concatMap collectFromExpr es
    collectFromExpr (ERecordCon _ fields _) = concatMap (collectFromExpr . snd) fields
    collectFromExpr (ERecordUpd e fields _) = collectFromExpr e ++ concatMap (collectFromExpr . snd) fields
    collectFromExpr (ENeg e _) = collectFromExpr e
    collectFromExpr (ETypeSig e _ _) = collectFromExpr e
    collectFromExpr _ = []

-- | SPEC-DIAG-005: Misleading choice name
--
-- Detects consuming choices whose name suggests read-only / validation
-- semantics (e.g., "Validate…", "Check…", "Verify…", "Query…", "Get…",
-- "Is…") but whose body performs state mutations (create, archive,
-- exercise).  This misleads developers and auditors about the side
-- effects of the choice.
misleadingChoiceName :: Inspection
misleadingChoiceName = mkInspection
  "SPEC-DIAG-005"
  "Misleading choice name"
  "A consuming choice has a name suggesting read-only/validation semantics but its body performs state mutations."
  Warning
  [Diagnostics]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-DIAG-005") Warning (chLocation ch)
        ("Template '" <> tplName tpl <> "': consuming choice '"
         <> chName ch <> "' has a validation/query name but performs state mutations ("
         <> T.intercalate ", " (mutationKinds (chBody ch))
         <> ") — name is misleading")
        (Just $ "Rename the choice to reflect its side effects, or make it nonconsuming if it truly only validates")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , chConsuming ch /= NonConsuming     -- only flag consuming choices
    , hasValidationPrefix (chName ch)    -- name suggests read-only
    , hasMutatingStatements (chBody ch)  -- body actually mutates state
    ]

-- | Does the choice name start with a validation/query-like prefix?
hasValidationPrefix :: Text -> Bool
hasValidationPrefix name =
  any (\p -> T.isPrefixOf p name) validationPrefixes
  where
    validationPrefixes =
      [ "Validate", "Check", "Verify", "Query"
      , "Is", "Lookup", "Read", "Inspect"
      ]

-- | Does the statement list contain any state-mutating operations?
hasMutatingStatements :: [Stmt] -> Bool
hasMutatingStatements = any isMutating
  where
    isMutating (SCreate _ _ _)            = True
    isMutating (SArchive _ _)             = True
    isMutating (SExercise _ _ _ _)        = True
    isMutating (SExerciseByKey _ _ _ _ _) = True
    isMutating (SBind _ e _)              = hasMutatingExpr e
    isMutating (SExpr e _)                = hasMutatingExpr e
    isMutating (SLet binds _)             = any (hasMutatingExpr . bindExpr) binds
    isMutating _                          = False

    hasMutatingExpr (EDo stmts _) = hasMutatingStatements stmts
    hasMutatingExpr (EApp f a _)  = hasMutatingExpr f || hasMutatingExpr a
    hasMutatingExpr (EParens e _) = hasMutatingExpr e
    hasMutatingExpr (EIf _ t e _) = hasMutatingExpr t || hasMutatingExpr e
    hasMutatingExpr (ECase _ alts _) = any (hasMutatingExpr . snd) alts
    hasMutatingExpr _             = False

-- | Describe which kinds of mutations are present in the statement list
mutationKinds :: [Stmt] -> [Text]
mutationKinds stmts = Set.toList $ Set.fromList $ concatMap classify stmts
  where
    classify (SCreate _ _ _)            = ["create"]
    classify (SArchive _ _)             = ["archive"]
    classify (SExercise _ _ _ _)        = ["exercise"]
    classify (SExerciseByKey _ _ _ _ _) = ["exercise"]
    classify (SBind _ e _)              = classifyExpr e
    classify (SExpr e _)                = classifyExpr e
    classify (SLet binds _)             = concatMap (classifyExpr . bindExpr) binds
    classify _                          = []

    classifyExpr (EDo stmts' _) = concatMap classify stmts'
    classifyExpr (EApp f a _)   = classifyExpr f ++ classifyExpr a
    classifyExpr (EParens e _)  = classifyExpr e
    classifyExpr (EIf _ t e _)  = classifyExpr t ++ classifyExpr e
    classifyExpr (ECase _ alts _) = concatMap (classifyExpr . snd) alts
    classifyExpr _              = []

-- | SPEC-DIAG-006: Consuming choice with trivial body (no successor contract)
--
-- A consuming choice whose body is trivial (just `pure ()` or `return ()`)
-- archives the contract without creating any audit record or successor.
-- The reason for the termination is lost. This is detected STRUCTURALLY:
-- any consuming choice with a trivial body, regardless of its name.
cancelWithoutAuditTrail :: Inspection
cancelWithoutAuditTrail = mkInspection
  "SPEC-DIAG-006"
  "Consuming choice with trivial body — no audit trail"
  "A consuming choice has a trivial body (pure ()) — the contract is archived without recording a reason or creating a successor/audit record."
  Warning
  [Diagnostics]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-DIAG-006") Warning (chLocation ch)
        ("Template '" <> tplName tpl <> "': consuming choice '"
         <> chName ch <> "' archives the contract without creating any successor or audit record")
        (Just "Create an audit/result record in the choice body, or accept a 'reason' parameter and persist it")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , chConsuming ch /= NonConsuming      -- must be consuming (archives)
    , isTrivialBody (chBody ch)           -- body is just pure () / return ()
    ]

-- | Is the choice body trivial — just `pure ()`, `return ()`, or empty?
-- A trivial body means the choice archives the contract (consuming) but
-- does nothing else of substance.
isTrivialBody :: [Stmt] -> Bool
isTrivialBody [] = True
isTrivialBody [SReturn (ETuple [] _) _] = True     -- return ()
isTrivialBody [SExpr e _] = isTrivialExpr e
isTrivialBody _ = False

-- | Is the expression trivially a no-op? (pure (), return ())
isTrivialExpr :: Expr -> Bool
isTrivialExpr (EApp (EVar "pure" _) (ETuple [] _) _) = True
isTrivialExpr (EApp (EVar "return" _) (ETuple [] _) _) = True
isTrivialExpr (EParens e _) = isTrivialExpr e
isTrivialExpr _ = False
