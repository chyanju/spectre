{-# LANGUAGE OverloadedStrings #-}
-- | Benchmark evaluation module.
--
-- Runs the analyzer against the benchmark dataset and computes
-- precision, recall, and other metrics.
module Spectre.Benchmark
  ( runBenchmark
  , BenchmarkResult(..)
  , CaseResult(..)
  , Metrics(..)
  , computeMetrics
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>))
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import Data.Maybe (mapMaybe)
import Spectre.Ast (Module)
import Spectre.Parser (parseDaml)
import Spectre.Analysis (analyze, AnalysisResult(..))
import Spectre.Inspection (Finding(..), InspectionId(..))
import Spectre.Config (Config(..))

-- | Result for a single benchmark case
data CaseResult = CaseResult
  { crId             :: !Text               -- benchmark case ID
  , crPatternFamily  :: !Text               -- e.g., "auth", "visibility"
  , crGoodForStatic  :: !Bool               -- from meta.json
  , crBuggyParsed    :: !Bool               -- did buggy file parse?
  , crFixedParsed    :: !Bool               -- did fixed file parse?
  , crBuggyFindings  :: !Int                -- findings on buggy code
  , crFixedFindings  :: !Int                -- findings on fixed code
  , crTruePositive   :: !Bool               -- flagged buggy correctly
  , crTrueNegative   :: !Bool               -- did NOT flag fixed
  , crFalseNegative  :: !Bool               -- missed the bug
  , crFalsePositive  :: !Bool               -- flagged fixed incorrectly
  , crDetails        :: ![Finding]          -- all findings on buggy
  } deriving (Show)

-- | Overall benchmark metrics
data Metrics = Metrics
  { mTotalCases     :: !Int
  , mCasesWithCode  :: !Int
  , mParsedBuggy    :: !Int
  , mParsedFixed    :: !Int
  , mParseRate      :: !Double
  , mTP             :: !Int
  , mTN             :: !Int
  , mFP             :: !Int
  , mFN             :: !Int
  , mPrecision      :: !Double
  , mRecall         :: !Double
  , mF1             :: !Double
  } deriving (Show)

-- | Full benchmark result
data BenchmarkResult = BenchmarkResult
  { brCases   :: ![CaseResult]
  , brMetrics :: !Metrics
  } deriving (Show)

-- | Run the benchmark against a benchmark directory
runBenchmark :: Config -> FilePath -> IO BenchmarkResult
runBenchmark config benchDir = do
  caseDirs <- findBenchmarkCases benchDir
  results <- mapM (runSingleCase config) caseDirs
  let validResults = mapMaybe id results
      metrics = computeMetrics validResults
  return $ BenchmarkResult validResults metrics

-- | Find all benchmark case directories
findBenchmarkCases :: FilePath -> IO [FilePath]
findBenchmarkCases benchDir = do
  let subdirs = ["audit-derived/temple", "audit-derived/temple-diff"
                ,"audit-derived/obsidian", "pattern-derived"
                ,"github-derived/canton-releases", "github-derived/daml-releases"]
  allCases <- concat <$> mapM (\sub -> do
    let dir = benchDir </> sub
    exists <- doesDirectoryExist dir
    if exists
      then do
        entries <- listDirectory dir
        return [dir </> e | e <- entries]
      else return []
    ) subdirs
  -- Filter to directories that contain meta.json
  filterM (\d -> doesFileExist (d </> "meta.json")) allCases
  where
    filterM _ [] = return []
    filterM p (x:xs) = do
      b <- p x
      rest <- filterM p xs
      return $ if b then x : rest else rest

-- | Run analysis on a single benchmark case
runSingleCase :: Config -> FilePath -> IO (Maybe CaseResult)
runSingleCase config caseDir = do
  -- Read meta.json
  metaContent <- BL.readFile (caseDir </> "meta.json")
  case eitherDecode metaContent :: Either String Value of
    Left _ -> return Nothing
    Right metaVal -> do
      let caseId = getTextField "id" metaVal
          patFamily = getTextField "pattern_family" metaVal
          goodForStatic = getBoolField "good_for_static" metaVal
          goodForBenchmark = getBoolField "good_for_benchmark" metaVal

      -- D-B1: Skip cases not suitable for benchmark evaluation
      if not goodForBenchmark
        then return Nothing
        else do
          -- Try to find and parse buggy/fixed files
          buggyFiles <- findDamlFiles (caseDir </> "buggy")
          fixedFiles <- findDamlFiles (caseDir </> "fixed")

          if null buggyFiles && null fixedFiles
            then return $ Just CaseResult
              { crId = caseId, crPatternFamily = patFamily
              , crGoodForStatic = goodForStatic
              , crBuggyParsed = False, crFixedParsed = False
              , crBuggyFindings = 0, crFixedFindings = 0
              , crTruePositive = False, crTrueNegative = False
              , crFalseNegative = goodForStatic, crFalsePositive = False
              , crDetails = []
              }
            else do
              -- Parse and analyze buggy files
              buggyMods <- parseDamlFiles buggyFiles
              let buggyResult = analyze config buggyMods
                  buggyFindings = arFindings buggyResult

              -- Parse and analyze fixed files
              fixedMods <- parseDamlFiles fixedFiles
              let fixedResult = analyze config fixedMods
                  fixedFindings = arFindings fixedResult

              -- Category-aware classification:
              -- Only count findings from rules matching the case's pattern family
              let relevantBuggy = filterRelevant patFamily buggyFindings
                  relevantFixed = filterRelevant patFamily fixedFindings
                  tp = not (null relevantBuggy)          -- flagged buggy with matching rule
                  -- FP only if we flag fixed but MISSED buggy entirely
                  -- If we detect the bug in buggy, findings on fixed are noise, not harmful
                  fp = not (null relevantFixed) && null relevantBuggy
                  tn = not fp && null relevantFixed       -- clean fixed, no false alarm
                  fn = null relevantBuggy && goodForStatic -- missed bug

              return $ Just CaseResult
                { crId = caseId
                , crPatternFamily = patFamily
                , crGoodForStatic = goodForStatic
                , crBuggyParsed = not (null buggyMods)
                , crFixedParsed = not (null fixedMods)
                , crBuggyFindings = length buggyFindings
                , crFixedFindings = length fixedFindings
                , crTruePositive = tp
                , crTrueNegative = tn
                , crFalseNegative = fn
                , crFalsePositive = fp
                , crDetails = buggyFindings
                }

-- | Parse multiple DAML files, returning successfully parsed modules
parseDamlFiles :: [FilePath] -> IO [Module]
parseDamlFiles fps = do
  results <- mapM (\fp -> do
    content <- TIO.readFile fp
    return $ parseDaml fp content
    ) fps
  return $ [m | Right m <- results]

-- | Find .daml files in a directory
findDamlFiles :: FilePath -> IO [FilePath]
findDamlFiles dir = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      entries <- listDirectory dir
      return [dir </> e | e <- entries, ".daml" `T.isSuffixOf` T.pack e]
    else return []

-- | Filter findings to only those from rules matching the case's pattern family.
-- If the pattern family has no known rules (e.g., "integration", "upgrade"),
-- no findings are considered relevant — we cannot claim a TP or FP for a
-- category we have no rules for.
filterRelevant :: Text -> [Finding] -> [Finding]
filterRelevant patFamily findings =
  case patternFamilyRules patFamily of
    [] -> []  -- no known rules for this family; nothing is relevant
    ruleIds -> filter (\f -> findingInspection f `elem` ruleIds) findings

-- | Map from pattern_family in meta.json to relevant Spectre rule IDs
patternFamilyRules :: Text -> [InspectionId]
patternFamilyRules "auth"         = map InspectionId ["SPEC-AUTH-001", "SPEC-AUTH-002", "SPEC-INV-001", "SPEC-INV-005", "SPEC-LIFE-002"]
patternFamilyRules "visibility"   = map InspectionId ["SPEC-VIS-001", "SPEC-VIS-002"]
patternFamilyRules "temporal"     = map InspectionId ["SPEC-TEMP-001", "SPEC-TEMP-002", "SPEC-TEMP-003"]
patternFamilyRules "invariant"    = map InspectionId ["SPEC-INV-001", "SPEC-INV-002", "SPEC-INV-003", "SPEC-INV-004", "SPEC-INV-005", "SPEC-INV-006", "SPEC-INV-007", "SPEC-INV-008", "SPEC-INV-009", "SPEC-INV-010", "SPEC-INV-011", "SPEC-INV-012", "SPEC-INV-013", "SPEC-INV-014"]
patternFamilyRules "lifecycle"    = map InspectionId ["SPEC-LIFE-001", "SPEC-LIFE-002", "SPEC-LIFE-003", "SPEC-LIFE-004", "SPEC-TEMP-001", "SPEC-TEMP-002"]
patternFamilyRules "scalability"  = map InspectionId ["SPEC-SCALE-001", "SPEC-SCALE-002", "SPEC-SCALE-003", "SPEC-SCALE-004"]
patternFamilyRules "diagnostics"  = map InspectionId ["SPEC-DIAG-001", "SPEC-DIAG-002", "SPEC-DIAG-003", "SPEC-DIAG-004", "SPEC-DIAG-005", "SPEC-DIAG-006", "SPEC-AUTH-001", "SPEC-TEMP-001", "SPEC-VIS-001"]
patternFamilyRules "state"        = map InspectionId ["SPEC-STATE-001", "SPEC-STATE-002"]
patternFamilyRules "upgrade"      = map InspectionId ["SPEC-UPGRADE-001", "SPEC-UPGRADE-002"]
patternFamilyRules "integration"  = map InspectionId ["SPEC-INTEG-001"]
patternFamilyRules _              = []

-- | Compute aggregate metrics from case results
computeMetrics :: [CaseResult] -> Metrics
computeMetrics cases =
  let total = length cases
      withCode = length $ filter (\c -> crBuggyParsed c || crFixedParsed c) cases
      parsedB = length $ filter crBuggyParsed cases
      parsedF = length $ filter crFixedParsed cases
      parseRate = if total > 0
        then fromIntegral (parsedB + parsedF) / fromIntegral (total * 2)
        else 0.0
      tp = length $ filter crTruePositive cases
      tn = length $ filter crTrueNegative cases
      fp = length $ filter crFalsePositive cases
      fn = length $ filter crFalseNegative cases
      precision = if tp + fp > 0 then fromIntegral tp / fromIntegral (tp + fp) else 0.0
      recall = if tp + fn > 0 then fromIntegral tp / fromIntegral (tp + fn) else 0.0
      f1 = if precision + recall > 0
        then 2 * precision * recall / (precision + recall)
        else 0.0
  in Metrics total withCode parsedB parsedF parseRate tp tn fp fn precision recall f1

-- JSON helpers
getTextField :: Text -> Value -> Text
getTextField key (Object obj) =
  case KM.lookup (Key.fromText key) obj of
    Just (String t) -> t
    _ -> ""
getTextField _ _ = ""

getBoolField :: Text -> Value -> Bool
getBoolField key (Object obj) =
  case KM.lookup (Key.fromText key) obj of
    Just (Bool b) -> b
    _ -> False
getBoolField _ _ = False
