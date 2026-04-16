{-# LANGUAGE OverloadedStrings #-}
-- | Configuration for the analysis engine.
module Spectre.Config
  ( Config(..)
  , defaultConfig
  , isRuleEnabled
  , OutputFormat(..)
  ) where

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set

import Spectre.Inspection (InspectionId(..))

-- | Output format selection
data OutputFormat = HumanReadable | JsonOutput
  deriving (Show, Eq)

-- | Configuration for analysis runs
data Config = Config
  { cfgEnabledRules  :: !(Maybe (Set Text))  -- Nothing = all rules
  , cfgDisabledRules :: !(Set Text)           -- explicitly disabled
  , cfgOutputFormat  :: !OutputFormat
  , cfgVerbose       :: !Bool
  , cfgBenchmarkDir  :: !(Maybe FilePath)     -- path to benchmark data
  } deriving (Show)

-- | Default configuration: all rules enabled, human-readable output
defaultConfig :: Config
defaultConfig = Config
  { cfgEnabledRules  = Nothing
  , cfgDisabledRules = Set.empty
  , cfgOutputFormat  = HumanReadable
  , cfgVerbose       = False
  , cfgBenchmarkDir  = Nothing
  }

-- | Check if a rule is enabled given the current configuration
isRuleEnabled :: Config -> InspectionId -> Bool
isRuleEnabled Config{..} (InspectionId rid) =
  let enabledOk = case cfgEnabledRules of
        Nothing -> True
        Just enabled -> rid `Set.member` enabled
      disabledOk = not (rid `Set.member` cfgDisabledRules)
  in enabledOk && disabledOk
