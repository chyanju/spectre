{-# LANGUAGE OverloadedStrings #-}
-- | Visibility-related inspection rules.
--
-- Checks for controller/observer mismatches and disclosure issues.
module Spectre.Rules.Visibility
  ( visibilityRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T

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
    , ch <- tplChoices tpl
    , ctrl <- chController ch
    , not (partyInList ctrl (tplSignatory tpl))
    , not (partyInList ctrl (tplObserver tpl))
    , not (isThisOrSelf ctrl)  -- skip if controller is 'this' (refers to template party)
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

-- | SPEC-VIS-002: Missing symmetric party in observer clause
--
-- Detects templates where the combined signatory+observer clauses reference
-- one side of a known paired-role pattern (e.g., ".sender" without
-- ".receiver", or ".lender" without ".borrower") but not the other.
-- In DAML, both counterparties typically need contract visibility to
-- participate in the workflow.
missingSymmetricObserver :: Inspection
missingSymmetricObserver = mkInspection
  "SPEC-VIS-002"
  "Missing symmetric party in observer clause"
  "Template observers/signatories reference one side of a party pair (e.g., sender) but not the counterpart (e.g., receiver), creating a visibility gap."
  Warning
  [Visibility]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-VIS-002") Warning (tplLocation tpl)
        ("Template '" <> tplName tpl
         <> "': signatory/observer references '" <> presentSuffix
         <> "' path but not '" <> missingSuffix
         <> "' — the counterparty cannot see this contract")
        (Just $ "Add the '" <> missingSuffix <> "' party to the observer clause")
        (Just (tplName tpl))
        Nothing
    | DTemplate tpl <- moduleDecls mod_
    , let allPartyExprs = tplSignatory tpl ++ tplObserver tpl
    , let allPaths = concatMap extractDottedPaths allPartyExprs
    , (presentSuffix, missingSuffix) <- findMissingSymmetricPaths allPaths
    ]

-- | Known symmetric party pairs.  If one suffix is present, the other
-- should also be present in the signatory/observer list.
symmetricPairs :: [(Text, Text)]
symmetricPairs =
  [ ("sender", "receiver")
  , ("buyer", "seller")
  , ("lender", "borrower")
  , ("payer", "payee")
  , ("maker", "taker")
  , ("transferor", "transferee")
  , ("grantor", "grantee")
  , ("issuer", "holder")
  ]

-- | Extract dotted path suffixes (last segment) from party expressions.
-- E.g., @PEField "allocation" "sender"@ → @["sender"]@
-- E.g., @PEExpr (EVar "allocation.transferLeg.sender")@ → @["sender"]@
extractDottedPaths :: PartyExpr -> [Text]
extractDottedPaths (PEField _ field _) = [T.toLower field]
extractDottedPaths (PEExpr (EVar v _) _) =
  case T.splitOn "." v of
    parts@(_:_:_) -> [T.toLower (last parts)]
    _ -> []
  where last xs = xs !! (length xs - 1)
extractDottedPaths (PEList ps _) = concatMap extractDottedPaths ps
extractDottedPaths _ = []

-- | Given a list of path suffixes present in signatory/observer,
-- find (present, missing) pairs from the symmetric pair table.
findMissingSymmetricPaths :: [Text] -> [(Text, Text)]
findMissingSymmetricPaths paths =
  let pathSet = map T.toLower paths
  in [ (a, b)
     | (a, b) <- symmetricPairs ++ map (\(x,y) -> (y,x)) symmetricPairs
     , a `elem` pathSet
     , b `notElem` pathSet
     ]
