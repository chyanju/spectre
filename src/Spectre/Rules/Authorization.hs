{-# LANGUAGE OverloadedStrings #-}
-- | Authorization-related inspection rules.
--
-- Checks for missing role/signatory/controller authorization.
module Spectre.Rules.Authorization
  ( authRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Spectre.Ast
import Spectre.Inspection
import Spectre.Rules.Utils (isPartySetField, isCollectionMemberCall, anyStmt, stmtAssertsCond, anyExpr)

-- | All authorization rules
authRules :: [Inspection]
authRules =
  [ signatoryAllowsUnilateralArchive
  , missingRoleMembershipCheck
  ]

-- | SPEC-AUTH-001: Signatory allows unilateral archive
--
-- Detects templates where an actor/user party is the sole signatory,
-- and the template is not consumable by any other party. This allows
-- the sole signatory to unilaterally archive (delete) records.
-- If the template is meant to be a shared fact or audit log, this is a flaw.
-- (This generalized version checks if the template lacks choices that might
-- represent multi-party workflows, and only has a single signatory).
signatoryAllowsUnilateralArchive :: Inspection
signatoryAllowsUnilateralArchive = mkInspection
  "SPEC-AUTH-001"
  "Signatory allows unilateral archive"
  "Template signatory model has only one signatory and lacks complex multi-party choices, allowing the sole signatory to unilaterally archive records. Consider requiring a governance party as signatory to protect shared facts."
  Warning
  [Authorization]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-AUTH-001") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl <> "' has a single signatory '"
         <> showPartyExpr p <> "' and no multi-party choices, allowing them to unilaterally archive")
        (Just "Add a governance/admin party as signatory to protect record integrity")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , [p] <- [tplSignatory tpl]  -- exactly one signatory
    , not (hasMultiPartyChoices tpl)
    , not (isObserverHeavy tpl)
    ]

-- | SPEC-AUTH-002: Missing role membership check in choice body
--
-- Detects choices where the controller is a parameter (not a template field)
-- and the choice body does not verify the caller's role membership before
-- executing privileged operations.
missingRoleMembershipCheck :: Inspection
missingRoleMembershipCheck = mkInspection
  "SPEC-AUTH-002"
  "Missing role membership check"
  "Choice controller is a parameter but the body does not verify role membership (e.g., Set.member check). Any party who can exercise this choice may bypass role-based access control."
  Error
  [Authorization]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-AUTH-002") Error (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "' has controller from parameter but no role membership check")
        (Just "Add 'assertMsg \"Missing role\" (Set.member caller roles)' before business logic")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , hasPartyCollectionField tpl
    , ch <- tplChoices tpl
    , controllerIsParam ch tpl
    , not (bodyHasRoleCheck (chBody ch))
    ]

-- Helpers

hasMultiPartyChoices :: Template -> Bool
hasMultiPartyChoices tpl =
  let choices = tplChoices tpl
  in length choices > 1 || any (\ch -> length (chController ch) > 1) choices

isObserverHeavy :: Template -> Bool
isObserverHeavy tpl = length (tplObserver tpl) > 1

showPartyExpr :: PartyExpr -> Text
showPartyExpr (PEVar name _) = name
showPartyExpr (PEField obj field _) = obj <> "." <> field
showPartyExpr (PEList ps _) = "[" <> T.intercalate ", " (map showPartyExpr ps) <> "]"
showPartyExpr (PEApp name _ _) = name <> "(...)"
showPartyExpr (PEExpr _ _) = "<expr>"

-- | Check if template has a field of type Set Party or [Party].
-- This is STRUCTURAL (type-based), not name-based.
hasPartyCollectionField :: Template -> Bool
hasPartyCollectionField tpl =
  any isPartySetField (tplFields tpl)

controllerIsParam :: Choice -> Template -> Bool
controllerIsParam ch tpl =
  -- Check if ANY controller is a choice parameter (not a template field)
  any isChoiceParam (chController ch)
  where
    isChoiceParam (PEVar name _) =
      any (\f -> fieldName f == name) (chParams ch)
      && not (any (\f -> fieldName f == name) (tplFields tpl))
    isChoiceParam _ = False

-- | Check if the choice body has any membership/lookup assertion.
-- Uses the universal assertion detector + collection membership call detection.
-- Does NOT hardcode "role" or "member" — instead checks for Set.member,
-- Map.member, elem, etc. via structural matching.
bodyHasRoleCheck :: [Stmt] -> Bool
bodyHasRoleCheck stmts =
  -- Check if any assertion condition calls a membership function
  any (stmtAssertsCond hasCollectionMembershipCall) stmts
  -- Also check for any membership call in the body at all (even outside assert)
  || anyStmt hasCollectionMembershipCall stmts

hasCollectionMembershipCall :: Expr -> Bool
hasCollectionMembershipCall = anyExpr isMemberCall
  where
    isMemberCall (EVar v _) = isCollectionMemberCall v
    isMemberCall (EApp (EVar v _) _ _) = isCollectionMemberCall v
    isMemberCall _ = False
