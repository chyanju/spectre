{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Core inspection and finding types for the analysis engine.
module Spectre.Inspection
  ( Inspection(..)
  , InspectionId(..)
  , Severity(..)
  , Category(..)
  , Finding(..)
  , mkInspection
  , mkFinding
  , severityToText
  , categoryToText
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=))

import Spectre.Ast (Module, SrcSpan(..))

-- | Unique inspection identifier (e.g., "SPEC-AUTH-001")
newtype InspectionId = InspectionId { unInspectionId :: Text }
  deriving (Show, Eq, Ord, Generic, FromJSON)

instance ToJSON InspectionId where
  toJSON (InspectionId t) = toJSON t

-- | Severity levels
data Severity = Info | Warning | Error
  deriving (Show, Eq, Ord, Generic, FromJSON)

instance ToJSON Severity where
  toJSON Info    = toJSON ("info" :: Text)
  toJSON Warning = toJSON ("warning" :: Text)
  toJSON Error   = toJSON ("error" :: Text)

-- | Categories of security findings
data Category
  = Authorization    -- signatory/controller/role checks
  | Visibility       -- observer/stakeholder visibility
  | Temporal         -- time/deadline checks
  | Invariant        -- data consistency, input validation
  | StateIntegrity   -- no-op updates, missing guards
  | Lifecycle        -- contract lifecycle issues
  | Scalability      -- unbounded growth, performance
  | CrossTemplate    -- cross-template consistency
  | Diagnostics      -- error messages, audit trail
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | An inspection rule definition
data Inspection = Inspection
  { inspId          :: !InspectionId
  , inspName        :: !Text
  , inspDescription :: !Text
  , inspSeverity    :: !Severity
  , inspCategories  :: ![Category]
  , inspCheck       :: Module -> [Finding]
  }

instance Show Inspection where
  show i = "Inspection " ++ show (inspId i) ++ " " ++ show (inspName i)

-- | A single finding (an inspection triggered at a location)
data Finding = Finding
  { findingInspection :: !InspectionId
  , findingSeverity   :: !Severity
  , findingLocation   :: !SrcSpan
  , findingMessage    :: !Text
  , findingSuggestion :: !(Maybe Text)
  , findingTemplate   :: !(Maybe Text)    -- which template
  , findingChoice     :: !(Maybe Text)    -- which choice (if applicable)
  } deriving (Show, Eq, Generic)

instance ToJSON Finding where
  toJSON Finding{..} = object
    [ "inspection" .= findingInspection
    , "severity"   .= findingSeverity
    , "location"   .= findingLocation
    , "message"    .= findingMessage
    , "suggestion" .= findingSuggestion
    , "template"   .= findingTemplate
    , "choice"     .= findingChoice
    ]

-- | Helper to create an inspection
mkInspection :: Text -> Text -> Text -> Severity -> [Category]
             -> (Module -> [Finding]) -> Inspection
mkInspection id_ name desc sev cats check = Inspection
  { inspId          = InspectionId id_
  , inspName        = name
  , inspDescription = desc
  , inspSeverity    = sev
  , inspCategories  = cats
  , inspCheck       = check
  }

-- | Helper to create a finding
mkFinding :: InspectionId -> Severity -> SrcSpan -> Text -> Maybe Text
          -> Maybe Text -> Maybe Text -> Finding
mkFinding = Finding

severityToText :: Severity -> Text
severityToText Info = "info"
severityToText Warning = "warning"
severityToText Error = "error"

categoryToText :: Category -> Text
categoryToText Authorization = "authorization"
categoryToText Visibility = "visibility"
categoryToText Temporal = "temporal"
categoryToText Invariant = "invariant"
categoryToText StateIntegrity = "state-integrity"
categoryToText Lifecycle = "lifecycle"
categoryToText Scalability = "scalability"
categoryToText CrossTemplate = "cross-template"
categoryToText Diagnostics = "diagnostics"
