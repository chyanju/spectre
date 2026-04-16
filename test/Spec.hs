{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (isRight)

import Spectre.Ast
import Spectre.Parser (parseDaml)
import Spectre.Inspection
import Spectre.Analysis (analyze, AnalysisResult(..))
import Spectre.Config (defaultConfig)

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    parserTests

  describe "Rules" $ do
    visibilityTests
    authorizationTests
    temporalTests
    invariantTests
    stateTests
    lifecycleTests

-- ============================================================
-- Parser Tests
-- ============================================================

parserTests :: Spec
parserTests = do
  it "parses a minimal template" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template Token"
          , "  with"
          , "    owner : Party"
          , "    amount : Decimal"
          , "  where"
          , "    signatory owner"
          ]
    parseDaml "<test>" src `shouldSatisfy` isRight

  it "parses a template with a choice" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template Token"
          , "  with"
          , "    owner : Party"
          , "    amount : Decimal"
          , "  where"
          , "    signatory owner"
          , ""
          , "    choice Transfer : ContractId Token"
          , "      with"
          , "        newOwner : Party"
          , "      controller owner"
          , "      do"
          , "        create this with owner = newOwner"
          ]
    parseDaml "<test>" src `shouldSatisfy` isRight

  it "parses a nonconsuming choice" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template Asset"
          , "  with"
          , "    issuer : Party"
          , "  where"
          , "    signatory issuer"
          , ""
          , "    nonconsuming choice GetInfo : Text"
          , "      controller issuer"
          , "      do"
          , "        return \"info\""
          ]
    parseDaml "<test>" src `shouldSatisfy` isRight

  it "extracts template name" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template MyToken"
          , "  with"
          , "    owner : Party"
          , "  where"
          , "    signatory owner"
          ]
    case parseDaml "<test>" src of
      Right mod_ -> do
        let templates = [t | DTemplate t <- moduleDecls mod_]
        length templates `shouldBe` 1
        tplName (head templates) `shouldBe` "MyToken"
      Left err -> expectationFailure $ "Parse failed: " ++ show err

-- ============================================================
-- Visibility Rule Tests
-- ============================================================

visibilityTests :: Spec
visibilityTests = describe "Visibility" $ do
  it "SPEC-VIS-001: detects controller not listed as observer (buggy)" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template Escrow"
          , "  with"
          , "    depositor : Party"
          , "    beneficiary : Party"
          , "  where"
          , "    signatory depositor"
          , ""
          , "    choice Release : ()"
          , "      controller beneficiary"
          , "      do"
          , "        return ()"
          ]
    let findings = analyzeSource src
    length findings `shouldSatisfy` (> 0)
    any (\f -> findingInspection f == InspectionId "SPEC-VIS-001") findings
      `shouldBe` True

  it "SPEC-VIS-001: no finding when controller is observer (fixed)" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template Escrow"
          , "  with"
          , "    depositor : Party"
          , "    beneficiary : Party"
          , "  where"
          , "    signatory depositor"
          , "    observer beneficiary"
          , ""
          , "    choice Release : ()"
          , "      controller beneficiary"
          , "      do"
          , "        return ()"
          ]
    let findings = analyzeSource src
    any (\f -> findingInspection f == InspectionId "SPEC-VIS-001") findings
      `shouldBe` False

  it "SPEC-VIS-001: no finding when controller is signatory" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template Token"
          , "  with"
          , "    owner : Party"
          , "  where"
          , "    signatory owner"
          , ""
          , "    choice Burn : ()"
          , "      controller owner"
          , "      do"
          , "        return ()"
          ]
    let findings = analyzeSource src
    any (\f -> findingInspection f == InspectionId "SPEC-VIS-001") findings
      `shouldBe` False

-- ============================================================
-- Authorization Rule Tests
-- ============================================================

authorizationTests :: Spec
authorizationTests = describe "Authorization" $ do
  it "SPEC-AUTH-001: detects actor as sole signatory on event template" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template AdminEvent"
          , "  with"
          , "    eventActor : Party"
          , "    eventGovernor : Party"
          , "    description : Text"
          , "  where"
          , "    signatory eventActor"
          , "    observer eventGovernor"
          ]
    let findings = analyzeSource src
    any (\f -> findingInspection f == InspectionId "SPEC-AUTH-001") findings
      `shouldBe` True

  it "SPEC-AUTH-001: no finding when governor is signatory (fixed)" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template AdminEvent"
          , "  with"
          , "    eventActor : Party"
          , "    eventGovernor : Party"
          , "    description : Text"
          , "  where"
          , "    signatory eventGovernor"
          , "    observer eventActor"
          ]
    let findings = analyzeSource src
    any (\f -> findingInspection f == InspectionId "SPEC-AUTH-001") findings
      `shouldBe` False

-- ============================================================
-- Temporal Rule Tests
-- ============================================================

temporalTests :: Spec
temporalTests = describe "Temporal" $ do
  it "SPEC-TEMP-001: detects missing deadline assertion" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template Settlement"
          , "  with"
          , "    owner : Party"
          , "    deadline : Time"
          , "  where"
          , "    signatory owner"
          , ""
          , "    choice Execute : ()"
          , "      controller owner"
          , "      do"
          , "        create this"
          , "        return ()"
          ]
    let findings = analyzeSource src
    -- Should detect that deadline field exists but no time assertion
    any (\f -> findingInspection f == InspectionId "SPEC-TEMP-001") findings
      `shouldBe` True

-- ============================================================
-- Invariant Rule Tests
-- ============================================================

invariantTests :: Spec
invariantTests = describe "Invariant" $ do
  it "SPEC-INV-002: detects use of head without guard" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template Batch"
          , "  with"
          , "    owner : Party"
          , "    items : [Text]"
          , "  where"
          , "    signatory owner"
          , ""
          , "    choice GetFirst : Text"
          , "      controller owner"
          , "      do"
          , "        return (head items)"
          ]
    let findings = analyzeSource src
    any (\f -> findingInspection f == InspectionId "SPEC-INV-002") findings
      `shouldBe` True

-- ============================================================
-- State Rule Tests
-- ============================================================

stateTests :: Spec
stateTests = describe "State" $ do
  it "SPEC-STATE-001: detects Set.insert without Set.member guard" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "import DA.Set (Set)"
          , "import qualified DA.Set as Set"
          , ""
          , "template Registry"
          , "  with"
          , "    admin : Party"
          , "    members : Set Party"
          , "  where"
          , "    signatory admin"
          , ""
          , "    choice AddMember : ContractId Registry"
          , "      with"
          , "        newMember : Party"
          , "      controller admin"
          , "      do"
          , "        create this with members = Set.insert newMember members"
          ]
    let findings = analyzeSource src
    any (\f -> findingInspection f == InspectionId "SPEC-STATE-001") findings
      `shouldBe` True

-- ============================================================
-- Lifecycle Rule Tests
-- ============================================================

lifecycleTests :: Spec
lifecycleTests = describe "Lifecycle" $ do
  it "SPEC-LIFE-001: detects nonconsuming choice that creates contracts" $ do
    let src = T.unlines
          [ "module Test where"
          , ""
          , "template Factory"
          , "  with"
          , "    owner : Party"
          , "  where"
          , "    signatory owner"
          , ""
          , "    nonconsuming choice Spawn : ContractId Token"
          , "      controller owner"
          , "      do"
          , "        create Token with issuer = owner"
          ]
    let findings = analyzeSource src
    any (\f -> findingInspection f == InspectionId "SPEC-LIFE-001") findings
      `shouldBe` True

-- ============================================================
-- Test Helpers
-- ============================================================

-- | Parse and analyze source code, returning all findings
analyzeSource :: Text -> [Finding]
analyzeSource src =
  case parseDaml "<test>" src of
    Left _ -> []
    Right mod_ -> arFindings $ analyze defaultConfig [mod_]
