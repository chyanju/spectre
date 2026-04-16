{-# LANGUAGE OverloadedStrings #-}
-- | Upgrade-related inspection rules.
--
-- Checks for DAML-specific patterns that hinder contract upgradeability,
-- such as choices returning raw ContractId types instead of wrapped
-- result types, or interface views that drop template fields.
module Spectre.Rules.Upgrade
  ( upgradeRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Spectre.Ast
import Spectre.Inspection

-- | All upgrade rules
upgradeRules :: [Inspection]
upgradeRules =
  [ rawContractIdReturn
  , interfaceViewFieldDrop
  ]

-- | SPEC-UPGRADE-001: Choice returns raw ContractId of own template
--
-- In DAML's upgrade model, changing a choice's return type is a breaking
-- change. Choices that return 'ContractId <SameTemplate>' directly couple
-- callers to the concrete template name.  Wrapping return values in
-- dedicated result data types (e.g., 'data MyChoice_Result = ...') allows
-- adding new return fields in future versions without breaking callers.
--
-- We flag templates where the *majority* (> 50%) of choices return a raw
-- ContractId of the same template.  A single choice doing so is common
-- for admin/update patterns and is acceptable.
rawContractIdReturn :: Inspection
rawContractIdReturn = mkInspection
  "SPEC-UPGRADE-001"
  "Choices return raw ContractId of own template"
  "Multiple choices return 'ContractId <TemplateName>' directly instead of a wrapped result type. This prevents non-breaking upgrades because changing a choice return type is a breaking change in DAML."
  Warning
  [Lifecycle]  -- Lifecycle is the closest existing category
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-UPGRADE-001") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl
         <> "': " <> T.pack (show rawCount) <> " of "
         <> T.pack (show totalChoices) <> " choices return raw 'ContractId "
         <> tplName tpl <> "' — hinders upgradeability")
        (Just "Wrap choice return types in dedicated data types (e.g., 'data ChoiceName_Result') to facilitate future smart contract upgrades")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , let totalChoices = length (tplChoices tpl)
    , totalChoices >= 3  -- only flag templates with at least 3 choices
    , let rawCount = length [ ch | ch <- tplChoices tpl
                            , returnsContractIdOfSelf (tplName tpl) ch
                            ]
    , rawCount >= 3  -- at least 3 choices returning raw ContractId Self
    , rawCount * 2 > totalChoices  -- majority of choices
    ]

-- | Check if a choice returns 'ContractId TemplateName' where TemplateName
-- matches the enclosing template.
returnsContractIdOfSelf :: Text -> Choice -> Bool
returnsContractIdOfSelf tplN ch = case chReturnType ch of
  Just (TApp (TCon "ContractId" _) (TCon name _) _) -> name == tplN
  _ -> False

-- | SPEC-UPGRADE-002: Interface view silently drops template fields
--
-- When a template implements an interface, the 'view' expression should
-- reference all semantically relevant template fields.  If a template
-- field is not mentioned anywhere in the view expression (neither as a
-- record field name nor as a variable reference in field values), consumers
-- fetching through the interface will receive incomplete data with no
-- error indication.
--
-- This is especially dangerous during template upgrades: a new field may
-- be added to the template but forgotten in the interface view.
interfaceViewFieldDrop :: Inspection
interfaceViewFieldDrop = mkInspection
  "SPEC-UPGRADE-002"
  "Interface view drops template fields"
  "A template field is not referenced in the interface view expression. Consumers fetching via the interface will receive incomplete data."
  Warning
  [Lifecycle]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-UPGRADE-002") Warning (ifaceSpan iface)
        ("Template '" <> tplName tpl <> "', interface '" <> ifaceName iface
         <> "': field '" <> droppedField
         <> "' is not referenced in the view expression — data is silently lost for interface consumers")
        (Just $ "Include '" <> droppedField <> "' in the interface view, possibly wrapped in Optional for backward compatibility")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , iface <- tplInterfaces tpl
    , Just viewExpr <- [ifaceViewExpr iface]
    , let viewVars = collectVarRefs viewExpr
    , let tplFieldNames = map fieldName (tplFields tpl)
    , droppedField <- tplFieldNames
    , droppedField `notElem` viewVars
    ]

-- | Collect all variable references (EVar names) from an expression,
-- recursively descending into sub-expressions.  Also collects field names
-- from ERecordCon (since field = field punning is a variable reference).
collectVarRefs :: Expr -> [Text]
collectVarRefs expr = case expr of
  EVar name _         -> [name]
  EApp f a _          -> collectVarRefs f ++ collectVarRefs a
  EInfix _ l r _      -> collectVarRefs l ++ collectVarRefs r
  ELam _ body _       -> collectVarRefs body
  ELet binds body _   -> concatMap (collectVarRefs . bindExpr) binds ++ collectVarRefs body
  EIf c t e _         -> collectVarRefs c ++ collectVarRefs t ++ collectVarRefs e
  ECase scrut alts _  -> collectVarRefs scrut ++ concatMap (collectVarRefs . snd) alts
  EDo stmts _         -> concatMap collectVarRefsStmt stmts
  EList es _          -> concatMap collectVarRefs es
  ETuple es _         -> concatMap collectVarRefs es
  ERecordCon _ flds _ -> concatMap (\(n, e) -> n : collectVarRefs e) flds
  ERecordUpd e flds _ -> collectVarRefs e ++ concatMap (\(_, v) -> collectVarRefs v) flds
  EFieldAccess e _ _  -> collectVarRefs e
  EParens e _         -> collectVarRefs e
  ETypeSig e _ _      -> collectVarRefs e
  ENeg e _            -> collectVarRefs e
  _                   -> []  -- EString, ENum, EWild, ELit

collectVarRefsStmt :: Stmt -> [Text]
collectVarRefsStmt stmt = case stmt of
  SBind _ e _       -> collectVarRefs e
  SLet binds _      -> concatMap (collectVarRefs . bindExpr) binds
  SCreate _ e _     -> collectVarRefs e
  SExercise e _ a _ -> collectVarRefs e ++ collectVarRefs a
  SFetch e _        -> collectVarRefs e
  SArchive e _      -> collectVarRefs e
  SAssert _ e _     -> collectVarRefs e
  SReturn e _       -> collectVarRefs e
  SExpr e _         -> collectVarRefs e
  _                 -> []
