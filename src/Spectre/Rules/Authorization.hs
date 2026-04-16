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

-- | All authorization rules
authRules :: [Inspection]
authRules =
  [ signatoryAllowsUnilateralArchive
  , missingRoleMembershipCheck
  ]

-- | SPEC-AUTH-001: Signatory allows unilateral archive
--
-- Detects templates where an actor/user party is the sole signatory,
-- allowing them to unilaterally archive (delete) audit/event records.
-- The fix is typically to make a governance party the signatory.
signatoryAllowsUnilateralArchive :: Inspection
signatoryAllowsUnilateralArchive = mkInspection
  "SPEC-AUTH-001"
  "Signatory allows unilateral archive"
  "Template signatory model allows a single actor to unilaterally archive records. Consider requiring a governance party as signatory to protect audit trails."
  Warning
  [Authorization]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-AUTH-001") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl <> "' has a single actor-like signatory '"
         <> showPartyExpr p <> "' who can unilaterally archive records")
        (Just "Add a governance/admin party as signatory to protect record integrity")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , isEventOrAuditTemplate tpl
    , [p] <- [tplSignatory tpl]  -- exactly one signatory
    , isActorLike p
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
    , hasRolesField tpl
    , ch <- tplChoices tpl
    , controllerIsParam ch tpl
    , not (bodyHasRoleCheck (chBody ch))
    ]

-- Helpers

isEventOrAuditTemplate :: Template -> Bool
isEventOrAuditTemplate tpl =
  let name = T.toLower (tplName tpl)
  in any (`T.isInfixOf` name) ["event", "audit", "log", "record", "history"]

isActorLike :: PartyExpr -> Bool
isActorLike (PEVar name _) = any (`T.isInfixOf` T.toLower name) ["actor", "caller", "sender", "user", "operator"]
isActorLike (PEField _ field _) = any (`T.isInfixOf` T.toLower field) ["actor", "caller", "sender", "user", "operator"]
isActorLike _ = False

showPartyExpr :: PartyExpr -> Text
showPartyExpr (PEVar name _) = name
showPartyExpr (PEField obj field _) = obj <> "." <> field
showPartyExpr (PEList ps _) = "[" <> T.intercalate ", " (map showPartyExpr ps) <> "]"
showPartyExpr (PEApp name _ _) = name <> "(...)"
showPartyExpr (PEExpr _ _) = "<expr>"

hasRolesField :: Template -> Bool
hasRolesField tpl =
  any (\f -> "role" `T.isInfixOf` T.toLower (fieldName f)) (tplFields tpl)

controllerIsParam :: Choice -> Template -> Bool
controllerIsParam ch tpl =
  -- Check if ANY controller is a choice parameter (not a template field)
  any isChoiceParam (chController ch)
  where
    isChoiceParam (PEVar name _) =
      any (\f -> fieldName f == name) (chParams ch)
      && not (any (\f -> fieldName f == name) (tplFields tpl))
    isChoiceParam _ = False

bodyHasRoleCheck :: [Stmt] -> Bool
bodyHasRoleCheck stmts = any isRoleCheck stmts
  where
    isRoleCheck (SAssert _ expr _) = exprMentions "member" expr || exprMentions "role" expr
    isRoleCheck (SExpr expr _) = isAssertWithRole expr
    isRoleCheck _ = False

    isAssertWithRole (EApp (EApp (EVar "assertMsg" _) _ _) cond _) =
      exprMentions "member" cond || exprMentions "role" cond
    isAssertWithRole _ = False

-- | Check if an expression mentions a given name anywhere
exprMentions :: Text -> Expr -> Bool
exprMentions name expr = case expr of
  EVar v _ -> T.isInfixOf (T.toLower name) (T.toLower v)
  EApp f a _ -> exprMentions name f || exprMentions name a
  EInfix _ l r _ -> exprMentions name l || exprMentions name r
  EFieldAccess e field _ -> T.isInfixOf (T.toLower name) (T.toLower field) || exprMentions name e
  EParens e _ -> exprMentions name e
  EIf c t e _ -> exprMentions name c || exprMentions name t || exprMentions name e
  _ -> False
