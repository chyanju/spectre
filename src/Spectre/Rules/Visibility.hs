{-# LANGUAGE OverloadedStrings #-}
-- | Visibility-related inspection rules.
--
-- Checks for controller/observer mismatches and disclosure issues.
module Spectre.Rules.Visibility
  ( visibilityRules
  ) where

import Data.Text (Text)

import Spectre.Ast
import Spectre.Inspection

-- | All visibility rules
visibilityRules :: [Inspection]
visibilityRules =
  [ controllerNotObserver
  , missingSymmetricObserver
  ]

-- | SPEC-VIS-001: Controller is not listed as observer or signatory
--
-- In DAML, a party must be a stakeholder (signatory or observer) to see
-- a contract. If a choice's controller is not a stakeholder, they cannot
-- see the contract and therefore cannot exercise the choice.
controllerNotObserver :: Inspection
controllerNotObserver = mkInspection
  "SPEC-VIS-001"
  "Controller not listed as observer"
  "A choice controller is not listed as either a signatory or observer of the template. The controller cannot see this contract and therefore cannot exercise this choice."
  Error
  [Visibility]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-VIS-001") Error (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': controller '" <> showPartyName ctrl
         <> "' is not listed as signatory or observer")
        (Just $ "Add '" <> showPartyName ctrl <> "' to the observer list of template '" <> tplName tpl <> "'")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , tplName tpl /= "_Snippet"  -- skip synthetic snippet templates (parser artifact with empty stakeholders)
    , ch <- tplChoices tpl
    , ctrl <- chController ch
    , not (partyInList ctrl (tplSignatory tpl))
    , not (partyInList ctrl (tplObserver tpl))
    , not (isThisOrSelf ctrl)  -- skip if controller is 'this' (refers to template party)
    , not (isChoiceParamWithStakeholderCoController ctrl ch (tplSignatory tpl ++ tplObserver tpl))
    ]

-- Helpers

showPartyName :: PartyExpr -> Text
showPartyName (PEVar name _) = name
showPartyName (PEField obj field _) = obj <> "." <> field
showPartyName (PEList _ _) = "[...]"
showPartyName (PEApp name _ _) = name <> "(...)"
showPartyName (PEExpr _ _) = "<expr>"

-- | Check if a party expression appears in a list of party expressions
partyInList :: PartyExpr -> [PartyExpr] -> Bool
partyInList _ [] = False
partyInList pe (PEList ps _ : rest) = any (partyMatches pe) ps || partyInList pe rest
partyInList pe (p : rest) = partyMatches pe p || partyInList pe rest

-- | Check if two party expressions refer to the same party
partyMatches :: PartyExpr -> PartyExpr -> Bool
partyMatches (PEVar a _) (PEVar b _) = a == b
partyMatches (PEField oa fa _) (PEField ob fb _) = oa == ob && fa == fb
partyMatches _ _ = False

-- | Check if party expression is 'this' or 'self'
isThisOrSelf :: PartyExpr -> Bool
isThisOrSelf (PEVar name _) = name == "this" || name == "self"
isThisOrSelf _ = False

-- =========================================================================
-- SPEC-VIS-002: Missing symmetric observer
-- =========================================================================

-- | SPEC-VIS-002: Choice controller not an observer (Asymmetric role)
--
-- Detects templates where a party is given control over a choice
-- (via controller) but is not explicitly made an observer or signatory.
-- While DAML semantics may sometimes implicitly allow this if the party
-- is a stakeholder on another contract being used, it often points to a
-- visibility gap where a counterparty cannot see the contract they are
-- supposed to interact with.
missingSymmetricObserver :: Inspection
missingSymmetricObserver = mkInspection
  "SPEC-VIS-002"
  "Choice controller is not a stakeholder (Asymmetric Role)"
  "A party is defined as a choice controller but is not an observer or signatory of the template, meaning they may not be able to see the contract to exercise the choice."
  Warning
  [Visibility]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-VIS-002") Warning (chLocation ch)
        ("Template '" <> tplName tpl
         <> "': Choice '" <> chName ch <> "' is controlled by '" <> showPartyName ctrl
         <> "' but this party is not in the signatory or observer lists.")
        (Just $ "Add '" <> showPartyName ctrl <> "' to the observer clause of template '" <> tplName tpl <> "'")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , tplName tpl /= "_Snippet"  -- skip synthetic snippet templates (parser artifact with empty stakeholders)
    , ch <- tplChoices tpl
    , ctrl <- chController ch
    , not (isThisOrSelf ctrl)
    , not (partyInStakeholders ctrl (tplSignatory tpl ++ tplObserver tpl))
    , not (isChoiceParamWithStakeholderCoController ctrl ch (tplSignatory tpl ++ tplObserver tpl))
    ]

-- | Deep check if a party expression is present in the stakeholders list
-- (Handles nested lists and dotted fields better)
partyInStakeholders :: PartyExpr -> [PartyExpr] -> Bool
partyInStakeholders pe stakeholders =
  let peStr = showPartyName pe
      stakeholdersStr = map showPartyName (flattenPartyExprs stakeholders)
  in peStr `elem` stakeholdersStr

flattenPartyExprs :: [PartyExpr] -> [PartyExpr]
flattenPartyExprs = concatMap go
  where
    go (PEList ps _) = flattenPartyExprs ps
    go p = [p]

-- | Check if a controller party expression refers to a choice parameter
-- AND at least one other co-controller is already a template stakeholder.
-- In Daml/Canton, when a choice has multiple controllers (e.g., `controller actor, operator`)
-- and at least one co-controller is a stakeholder, the non-stakeholder controller
-- can exercise the choice because the stakeholder provides the necessary visibility
-- context (they must co-sign the exercise). A choice-param controller that is the
-- SOLE controller with no stakeholder co-controller is still a genuine visibility concern.
isChoiceParamWithStakeholderCoController :: PartyExpr -> Choice -> [PartyExpr] -> Bool
isChoiceParamWithStakeholderCoController (PEVar name _) ch stakeholders =
  name `elem` map fieldName (chParams ch)
  && any isStakeholderCoController (chController ch)
  where
    isStakeholderCoController ctrl =
      not (isSameVar ctrl) && partyInList ctrl stakeholders
    isSameVar (PEVar n _) = n == name
    isSameVar _ = False
isChoiceParamWithStakeholderCoController _ _ _ = False
