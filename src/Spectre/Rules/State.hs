{-# LANGUAGE OverloadedStrings #-}
-- | State integrity inspection rules.
--
-- Checks for asymmetric set operations and missing guards.
module Spectre.Rules.State
  ( stateRules
  ) where

import Data.Text (Text)

import Spectre.Ast
import Spectre.Inspection

-- | All state integrity rules
stateRules :: [Inspection]
stateRules =
  [ setInsertWithoutMemberGuard
  , asymmetricAddRemove
  ]

-- | SPEC-STATE-001: Set.insert without Set.member guard
--
-- Detects choices that call Set.insert on a set field without first
-- checking Set.member, which can lead to duplicate entries or
-- no-op state updates.
setInsertWithoutMemberGuard :: Inspection
setInsertWithoutMemberGuard = mkInspection
  "SPEC-STATE-001"
  "Set.insert without membership guard"
  "Choice calls Set.insert without first checking Set.member. This can lead to silent no-op state updates or duplicate entries."
  Warning
  [StateIntegrity]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-STATE-001") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': Set.insert called without prior Set.member check")
        (Just "Add 'assertMsg \"already exists\" (not (Set.member x set))' before Set.insert")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , bodyHasSetInsert (chBody ch)
    , not (bodyHasSetMemberCheck (chBody ch))
    ]

-- | SPEC-STATE-002: Asymmetric add/remove operations
--
-- Detects templates where choices include add/insert operations
-- but no corresponding remove/delete operations (or vice versa).
asymmetricAddRemove :: Inspection
asymmetricAddRemove = mkInspection
  "SPEC-STATE-002"
  "Asymmetric add/remove operations"
  "Template has choices that add to a collection but no corresponding choices that remove from it, potentially leading to unbounded growth."
  Info
  [StateIntegrity, Scalability]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-STATE-002") Info (tplLocation tpl)
        ("Template '" <> tplName tpl
         <> "': has Set.insert/Map.insert in choices but no corresponding delete/remove")
        (Just "Add a corresponding remove/delete choice, or document why removal is not needed")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , hasInsertChoices tpl
    , not (hasDeleteChoices tpl)
    ]

-- Helpers

bodyHasSetInsert :: [Stmt] -> Bool
bodyHasSetInsert = any stmtHasSetInsert
  where
    stmtHasSetInsert (SExpr e _) = exprHasCall "Set.insert" e || exprHasCall "Map.insert" e
    stmtHasSetInsert (SBind _ e _) = exprHasCall "Set.insert" e || exprHasCall "Map.insert" e
    stmtHasSetInsert (SLet binds _) = any ((\e -> exprHasCall "Set.insert" e || exprHasCall "Map.insert" e) . bindExpr) binds
    stmtHasSetInsert (SCreate _ e _) = exprHasCall "Set.insert" e || exprHasCall "Map.insert" e
    stmtHasSetInsert _ = False

bodyHasSetMemberCheck :: [Stmt] -> Bool
bodyHasSetMemberCheck = any check
  where
    check (SAssert _ e _) = exprHasCall "Set.member" e || exprHasCall "Map.member" e
    check (SExpr e _) = exprHasCall "Set.member" e || exprHasCall "Map.member" e
    check _ = False

exprHasCall :: Text -> Expr -> Bool
exprHasCall name (EVar v _) = v == name
exprHasCall name (EApp f a _) = exprHasCall name f || exprHasCall name a
exprHasCall name (EInfix _ l r _) = exprHasCall name l || exprHasCall name r
exprHasCall name (EParens e _) = exprHasCall name e
exprHasCall name (EFieldAccess e _ _) = exprHasCall name e
exprHasCall name (ERecordCon _ fields _) = any (exprHasCall name . snd) fields
exprHasCall name (ERecordUpd e fields _) = exprHasCall name e || any (exprHasCall name . snd) fields
exprHasCall name (EIf c t e _) = exprHasCall name c || exprHasCall name t || exprHasCall name e
exprHasCall name (ELet binds body _) = any (exprHasCall name . bindExpr) binds || exprHasCall name body
exprHasCall _ _ = False

hasInsertChoices :: Template -> Bool
hasInsertChoices tpl = any (bodyHasSetInsert . chBody) (tplChoices tpl)

hasDeleteChoices :: Template -> Bool
hasDeleteChoices tpl = any hasDelete (tplChoices tpl)
  where
    hasDelete ch = any stmtHasDelete (chBody ch)
    stmtHasDelete (SExpr e _) = exprHasCall "Set.delete" e || exprHasCall "Map.delete" e
    stmtHasDelete (SBind _ e _) = exprHasCall "Set.delete" e || exprHasCall "Map.delete" e
    stmtHasDelete (SCreate _ e _) = exprHasCall "Set.delete" e || exprHasCall "Map.delete" e
    stmtHasDelete _ = False
