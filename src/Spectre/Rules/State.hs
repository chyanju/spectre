{-# LANGUAGE OverloadedStrings #-}
-- | State integrity inspection rules.
--
-- Checks for asymmetric set operations and missing guards.
module Spectre.Rules.State
  ( stateRules
  ) where

import Spectre.Ast
import Spectre.Inspection
import Spectre.Rules.Utils (isCollectionInsertCall, isCollectionDeleteCall, isCollectionMemberCall, anyStmt)

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
bodyHasSetInsert = anyStmt isInsertCallExpr

bodyHasSetMemberCheck :: [Stmt] -> Bool
bodyHasSetMemberCheck = anyStmt isMemberCallExpr

-- | Structural detection of insert calls (handles any qualified name)
isInsertCallExpr :: Expr -> Bool
isInsertCallExpr (EVar v _) = isCollectionInsertCall v
isInsertCallExpr (EApp (EVar v _) _ _) = isCollectionInsertCall v
isInsertCallExpr _ = False

-- | Structural detection of member/lookup calls
isMemberCallExpr :: Expr -> Bool
isMemberCallExpr (EVar v _) = isCollectionMemberCall v
isMemberCallExpr (EApp (EVar v _) _ _) = isCollectionMemberCall v
isMemberCallExpr _ = False

hasInsertChoices :: Template -> Bool
hasInsertChoices tpl = any (bodyHasSetInsert . chBody) (tplChoices tpl)

hasDeleteChoices :: Template -> Bool
hasDeleteChoices tpl = any hasDelete (tplChoices tpl)
  where
    hasDelete ch = anyStmt isDeleteCallExpr (chBody ch)

isDeleteCallExpr :: Expr -> Bool
isDeleteCallExpr (EVar v _) = isCollectionDeleteCall v
isDeleteCallExpr (EApp (EVar v _) _ _) = isCollectionDeleteCall v
isDeleteCallExpr _ = False
