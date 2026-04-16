{-# LANGUAGE OverloadedStrings #-}
-- | Analysis engine that runs inspections against parsed DAML modules.
module Spectre.Analysis
  ( analyze
  , analyzeModule
  , analyzeModules
  , AnalysisResult(..)
  ) where

import Data.Text (Text)
import Data.List (sortOn)
import Data.Ord (Down(..))

import Spectre.Ast
import Spectre.Inspection
import Spectre.Rules.All (allInspections)
import Spectre.Config (Config(..), isRuleEnabled)

-- | Result of analyzing one or more modules
data AnalysisResult = AnalysisResult
  { arFindings    :: ![Finding]
  , arFileCount   :: !Int
  , arRulesRun    :: !Int
  , arParseErrors :: ![(FilePath, Text)]
  } deriving (Show)

-- | Analyze a single module with all enabled inspections
analyzeModule :: Config -> Module -> [Finding]
analyzeModule config mod_ =
  let enabled = filter (isRuleEnabled config . inspId) allInspections
  in concatMap (\insp -> inspCheck insp mod_) enabled

-- | Analyze multiple modules
analyzeModules :: Config -> [Module] -> AnalysisResult
analyzeModules config mods =
  let enabled = filter (isRuleEnabled config . inspId) allInspections
      findings = concatMap (\mod_ -> concatMap (\i -> inspCheck i mod_) enabled) mods
      sorted = sortOn (Down . findingSeverity) findings
  in AnalysisResult
    { arFindings    = sorted
    , arFileCount   = length mods
    , arRulesRun    = length enabled
    , arParseErrors = []
    }

-- | Main analysis entry point (convenience wrapper)
analyze :: Config -> [Module] -> AnalysisResult
analyze = analyzeModules
