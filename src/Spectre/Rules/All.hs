{-# LANGUAGE OverloadedStrings #-}
-- | Registry of all inspection rules.
module Spectre.Rules.All
  ( allInspections
  , allInspectionIds
  ) where

import Spectre.Inspection
import Spectre.Rules.Authorization (authRules)
import Spectre.Rules.Visibility (visibilityRules)
import Spectre.Rules.Temporal (temporalRules)
import Spectre.Rules.Invariant (invariantRules)
import Spectre.Rules.State (stateRules)
import Spectre.Rules.Lifecycle (lifecycleRules)
import Spectre.Rules.Scalability (scalabilityRules)
import Spectre.Rules.CrossTemplate (crossTemplateRules)
import Spectre.Rules.Upgrade (upgradeRules)
import Spectre.Rules.Integration (integrationRules)

-- | All registered inspections (38 rules total)
allInspections :: [Inspection]
allInspections = concat
  [ authRules         -- SPEC-AUTH-001, SPEC-AUTH-002                                    (2)
  , visibilityRules   -- SPEC-VIS-001, SPEC-VIS-002                                     (2)
  , temporalRules     -- SPEC-TEMP-001..SPEC-TEMP-003                                   (3)
  , invariantRules    -- SPEC-INV-001..SPEC-INV-013                                     (13)
  , stateRules        -- SPEC-STATE-001, SPEC-STATE-002                                 (2)
  , lifecycleRules    -- SPEC-LIFE-001..SPEC-LIFE-004                                   (4)
  , scalabilityRules  -- SPEC-SCALE-001..SPEC-SCALE-003                                 (3)
  , crossTemplateRules -- SPEC-DIAG-001..SPEC-DIAG-006                                  (6)
  , upgradeRules      -- SPEC-UPGRADE-001, SPEC-UPGRADE-002                             (2)
  , integrationRules  -- SPEC-INTEG-001                                                 (1)
  ]

-- | All inspection IDs for reference
allInspectionIds :: [InspectionId]
allInspectionIds = map inspId allInspections
