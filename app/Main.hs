{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr, stdout, hIsTerminalDevice)
import System.Environment (lookupEnv)
import System.Directory (doesFileExist)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative
import Text.Megaparsec.Error (errorBundlePretty)

import Spectre.Parser (parseDamlFile)
import qualified Spectre.Parser (ParseError)
import Spectre.Analysis (analyze, AnalysisResult(..))
import Spectre.Inspection (Finding(..), Severity(..), InspectionId(..), inspId, inspName, inspDescription, inspSeverity)
import Spectre.Config (Config(..), defaultConfig, OutputFormat(..))
import Spectre.Report (renderFindings)
import Spectre.Rules.All (allInspections, allInspectionIds)
import Spectre.Benchmark (runBenchmark, BenchmarkResult(..), Metrics(..), CaseResult(..))

-- | CLI command structure
data Command
  = Analyze AnalyzeOpts
  | Benchmark BenchmarkOpts
  | ListRules
  deriving (Show)

data AnalyzeOpts = AnalyzeOpts
  { analyzeFiles   :: [FilePath]
  , analyzeFormat  :: OutputFormat
  , analyzeRules   :: Maybe [Text]
  , analyzeDisable :: [Text]
  , analyzeVerbose :: Bool
  } deriving (Show)

data BenchmarkOpts = BenchmarkOpts
  { benchDir     :: FilePath
  , benchFormat  :: OutputFormat
  , benchVerbose :: Bool
  } deriving (Show)

-- | CLI parser
opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
  <> header "spectre - DAML smart contract security analyzer"
  <> progDesc "Static analysis tool for detecting security bugs in DAML contracts"
  )

commandParser :: Parser Command
commandParser = subparser
  ( command "analyze"
    (info (Analyze <$> analyzeParser)
      (progDesc "Analyze DAML source files for security issues"))
  <> command "benchmark"
    (info (Benchmark <$> benchmarkParser)
      (progDesc "Run analysis against the benchmark dataset"))
  <> command "rules"
    (info (pure ListRules)
      (progDesc "List all available inspection rules"))
  )

analyzeParser :: Parser AnalyzeOpts
analyzeParser = AnalyzeOpts
  <$> some (argument str (metavar "FILES..." <> help "DAML source files to analyze"))
  <*> flag HumanReadable JsonOutput
      (long "json" <> help "Output results as JSON")
  <*> optional (some (strOption (long "rule" <> short 'r' <> metavar "RULE"
      <> help "Enable only specific rules (can be repeated)")))
  <*> many (strOption (long "disable" <> short 'd' <> metavar "RULE"
      <> help "Disable specific rules (can be repeated)"))
  <*> switch (long "verbose" <> short 'v' <> help "Verbose output")

benchmarkParser :: Parser BenchmarkOpts
benchmarkParser = BenchmarkOpts
  <$> strOption (long "dir" <> short 'd' <> metavar "DIR"
      <> value "benchmarks" <> help "Path to benchmark directory")
  <*> flag HumanReadable JsonOutput
      (long "json" <> help "Output results as JSON")
  <*> switch (long "verbose" <> short 'v' <> help "Verbose output")

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Analyze aopts -> runAnalyze aopts
    Benchmark bopts -> runBench bopts
    ListRules -> listRules

-- | Run analyze command
runAnalyze :: AnalyzeOpts -> IO ()
runAnalyze AnalyzeOpts{..} = do
  -- B3: Validate --rule names against known inspections
  let knownIds = Set.fromList [rid | InspectionId rid <- allInspectionIds]
  case analyzeRules of
    Just rules -> do
      let unknown = filter (\r -> not (r `Set.member` knownIds)) rules
      mapM_ (\r -> hPutStrLn stderr $ "Warning: unknown rule '" ++ T.unpack r ++ "' (ignored)") unknown
    Nothing -> return ()

  let config = defaultConfig
        { cfgEnabledRules = fmap (Set.fromList) analyzeRules
        , cfgDisabledRules = Set.fromList analyzeDisable
        , cfgOutputFormat = analyzeFormat
        , cfgVerbose = analyzeVerbose
        }

  -- A4: Check file existence before parsing
  existChecks <- mapM (\f -> do
    e <- doesFileExist f
    if e then return (Right f)
    else do
      hPutStrLn stderr $ "Error: file not found: " ++ f
      return (Left f)
    ) analyzeFiles
  let validFiles = [f | Right f <- existChecks]
      missingFiles = [f | Left f <- existChecks]

  if null validFiles && not (null missingFiles)
    then exitWith (ExitFailure 2)  -- all files missing
    else return ()

  -- B2: Verbose — show per-file parse status and enabled rules
  let enabledRules = filter (\i -> case cfgEnabledRules config of
                          Nothing -> not (unInspectionId (inspId i) `Set.member` cfgDisabledRules config)
                          Just en -> unInspectionId (inspId i) `Set.member` en
                                  && not (unInspectionId (inspId i) `Set.member` cfgDisabledRules config)
                     ) allInspections
  if analyzeVerbose
    then do
      hPutStrLn stderr $ "Spectre v0.1.3.0 — analyzing " ++ show (length validFiles) ++ " file(s) with " ++ show (length enabledRules) ++ " rules"
      mapM_ (\i -> hPutStrLn stderr $ "  [enabled] " ++ T.unpack (unInspectionId (inspId i))) enabledRules
    else return ()

  modules <- mapM parseDamlFile validFiles
  let (parsed, errors) = partitionResults modules validFiles

  -- B2: Verbose — show per-file parse results
  if analyzeVerbose
    then do
      mapM_ (\(f, _) -> hPutStrLn stderr $ "  [parsed]  " ++ f) (zip validFiles modules)
      mapM_ (\(f, e) -> hPutStrLn stderr $ "  [FAIL]    " ++ f ++ ": " ++ T.unpack e) errors
    else return ()

  -- B5: Parse errors to stderr
  mapM_ (\(f, e) -> hPutStrLn stderr $ f ++ ": " ++ T.unpack e) errors

  let result = (analyze config parsed)
        { arParseErrors = errors }
  useColor <- shouldUseColor
  TIO.putStr $ renderFindings useColor analyzeFormat result
  let errorCount = length $ filter (\f -> findingSeverity f == Error) (arFindings result)
  if errorCount > 0
    then exitWith (ExitFailure 1)
    else exitWith ExitSuccess

-- | Run benchmark command
runBench :: BenchmarkOpts -> IO ()
runBench BenchmarkOpts{..} = do
  let config = defaultConfig
        { cfgOutputFormat = benchFormat
        , cfgVerbose = benchVerbose
        }
  result <- runBenchmark config benchDir
  -- A6: Support --json output for benchmarks
  case benchFormat of
    JsonOutput -> do
      let m = brMetrics result
          jsonVal = object
            [ "metrics" .= object
                [ "total_cases"     .= mTotalCases m
                , "cases_with_code" .= mCasesWithCode m
                , "parsed_buggy"    .= mParsedBuggy m
                , "parsed_fixed"    .= mParsedFixed m
                , "parse_rate"      .= mParseRate m
                , "true_positives"  .= mTP m
                , "true_negatives"  .= mTN m
                , "false_positives" .= mFP m
                , "false_negatives" .= mFN m
                , "precision"       .= mPrecision m
                , "recall"          .= mRecall m
                , "f1_score"        .= mF1 m
                ]
            , "cases" .= map caseToJSON (brCases result)
            ]
      TIO.putStrLn $ TE.decodeUtf8 $ BL.toStrict $ encodePretty jsonVal
    HumanReadable -> do
      let m = brMetrics result
      TIO.putStrLn "Spectre Benchmark Evaluation"
      TIO.putStrLn "============================"
      TIO.putStrLn $ "Total cases:    " <> T.pack (show (mTotalCases m))
      TIO.putStrLn $ "Cases w/ code:  " <> T.pack (show (mCasesWithCode m))
      TIO.putStrLn $ "Parsed buggy:   " <> T.pack (show (mParsedBuggy m))
      TIO.putStrLn $ "Parsed fixed:   " <> T.pack (show (mParsedFixed m))
      TIO.putStrLn $ "Parse rate:     " <> T.pack (showPct (mParseRate m))
      TIO.putStrLn ""
      TIO.putStrLn $ "True Positives:  " <> T.pack (show (mTP m))
      TIO.putStrLn $ "True Negatives:  " <> T.pack (show (mTN m))
      TIO.putStrLn $ "False Positives: " <> T.pack (show (mFP m))
      TIO.putStrLn $ "False Negatives: " <> T.pack (show (mFN m))
      TIO.putStrLn ""
      TIO.putStrLn $ "Precision:      " <> T.pack (showPct (mPrecision m))
      TIO.putStrLn $ "Recall:         " <> T.pack (showPct (mRecall m))
      TIO.putStrLn $ "F1 Score:       " <> T.pack (showPct (mF1 m))

      if benchVerbose
        then do
          TIO.putStrLn "\nDetailed Results:"
          TIO.putStrLn "─────────────────"
          mapM_ printCaseResult (brCases result)
        else return ()

-- | List all available rules
listRules :: IO ()
listRules = do
  TIO.putStrLn "Available Inspection Rules:"
  TIO.putStrLn "==========================="
  mapM_ (\insp -> do
    let InspectionId rid = inspId insp
    TIO.putStrLn $ "  " <> rid <> " [" <> T.pack (show (inspSeverity insp)) <> "]"
    TIO.putStrLn $ "    " <> inspName insp
    TIO.putStrLn $ "    " <> inspDescription insp
    TIO.putStrLn ""
    ) allInspections

-- Helpers

partitionResults :: [Either Spectre.Parser.ParseError a] -> [FilePath] -> ([a], [(FilePath, Text)])
partitionResults results files = go results files ([], [])
  where
    go [] _ (mods, errs) = (reverse mods, reverse errs)
    go _ [] (mods, errs) = (reverse mods, reverse errs)
    go (Right m : rs) (_ : fs) (mods, errs) = go rs fs (m : mods, errs)
    go (Left e : rs) (f : fs) (mods, errs) = go rs fs (mods, (f, T.pack (errorBundlePretty e)) : errs)

printCaseResult :: CaseResult -> IO ()
printCaseResult CaseResult{..} = do
  let fpTag = if crFalsePositive then "+FP" else ""
      fnTag = if crFalseNegative then "+FN" else ""
      status
        | crTruePositive && crTrueNegative = "TP+TN" <> fpTag <> fnTag
        | crTruePositive = "TP" <> fpTag <> fnTag
        | crTrueNegative = "TN" <> fpTag <> fnTag
        | crFalseNegative = "FN" <> fpTag
        | crFalsePositive = "FP" <> fnTag
        | otherwise = "??" <> fpTag <> fnTag
      parsed = if crBuggyParsed then "parsed" else "PARSE-FAIL"
  TIO.putStrLn $ "  " <> crId <> " [" <> crPatternFamily <> "] "
    <> status <> " (" <> parsed <> ", "
    <> T.pack (show crBuggyFindings) <> " buggy / "
    <> T.pack (show crFixedFindings) <> " fixed findings)"

showPct :: Double -> String
showPct d = show (round (d * 100) :: Int) ++ "%"

-- | Detect whether to use ANSI colors: must be a TTY and NO_COLOR not set
shouldUseColor :: IO Bool
shouldUseColor = do
  isTty <- hIsTerminalDevice stdout
  noColor <- lookupEnv "NO_COLOR"
  return $ isTty && noColor == Nothing

-- | Convert a CaseResult to JSON
caseToJSON :: CaseResult -> Value
caseToJSON CaseResult{..} = object
  [ "id"              .= crId
  , "pattern_family"  .= crPatternFamily
  , "good_for_static" .= crGoodForStatic
  , "buggy_parsed"    .= crBuggyParsed
  , "fixed_parsed"    .= crFixedParsed
  , "buggy_findings"  .= crBuggyFindings
  , "fixed_findings"  .= crFixedFindings
  , "true_positive"   .= crTruePositive
  , "true_negative"   .= crTrueNegative
  , "false_negative"  .= crFalseNegative
  , "false_positive"  .= crFalsePositive
  ]
