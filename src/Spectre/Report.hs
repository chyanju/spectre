{-# LANGUAGE OverloadedStrings #-}
-- | Output formatting for analysis results.
module Spectre.Report
  ( renderHuman
  , renderJson
  , renderFindings
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (toJSON, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)

import Spectre.Ast (SrcSpan(..))
import Spectre.Inspection
import Spectre.Analysis (AnalysisResult(..))
import Spectre.Config (OutputFormat(..))

-- ────────────────────────────────────────────────
-- ANSI color helpers
-- ────────────────────────────────────────────────

type ColorFn = Text -> Text

data Theme = Theme
  { cBold    :: !ColorFn
  , cRed     :: !ColorFn
  , cYellow  :: !ColorFn
  , cCyan    :: !ColorFn
  , cGreen   :: !ColorFn
  , cBlue    :: !ColorFn
  , cDim     :: !ColorFn
  , cReset   :: !Text
  }

colorTheme :: Theme
colorTheme = Theme
  { cBold   = ansi "1"
  , cRed    = ansi "1;31"
  , cYellow = ansi "33"
  , cCyan   = ansi "36"
  , cGreen  = ansi "32"
  , cBlue   = ansi "34"
  , cDim    = ansi "2"
  , cReset  = "\x1b[0m"
  }

noColorTheme :: Theme
noColorTheme = Theme
  { cBold = id, cRed = id, cYellow = id, cCyan = id
  , cGreen = id, cBlue = id, cDim = id, cReset = ""
  }

ansi :: Text -> Text -> Text
ansi code t = "\x1b[" <> code <> "m" <> t <> "\x1b[0m"

-- ────────────────────────────────────────────────
-- Public API
-- ────────────────────────────────────────────────

-- | Render findings based on output format.
--   The Bool controls ANSI color (True = color, False = plain).
renderFindings :: Bool -> OutputFormat -> AnalysisResult -> Text
renderFindings _     JsonOutput     = renderJson
renderFindings color HumanReadable  = renderHuman (if color then colorTheme else noColorTheme)

-- | Render results as human-readable text
renderHuman :: Theme -> AnalysisResult -> Text
renderHuman th AnalysisResult{..} =
  let header = cBold th ("Spectre DAML Security Analysis\n")
            <> cDim th "==============================" <> "\n"
            <> "Files analyzed: " <> cBold th (T.pack (show arFileCount)) <> "\n"
            <> "Rules applied:  " <> cBold th (T.pack (show arRulesRun)) <> "\n"
            <> "Findings:       " <> cBold th (T.pack (show (length arFindings))) <> "\n"
            <> "\n"
      errors = renderParseErrors th arParseErrors
      findings = T.intercalate "\n" (map (renderFinding th) arFindings)
      summary = renderSummary th arFindings
  in header <> errors <> findings <> "\n" <> summary

-- | Render results as JSON
renderJson :: AnalysisResult -> Text
renderJson AnalysisResult{..} =
  let errors = length $ filter (\f -> findingSeverity f == Error) arFindings
      warnings = length $ filter (\f -> findingSeverity f == Warning) arFindings
      infos = length $ filter (\f -> findingSeverity f == Info) arFindings
      jsonVal = object
        [ "files_analyzed" .= arFileCount
        , "rules_applied"  .= arRulesRun
        , "finding_count"  .= length arFindings
        , "summary" .= object
            [ "errors"   .= errors
            , "warnings" .= warnings
            , "info"     .= infos
            ]
        , "findings"       .= map toJSON arFindings
        , "parse_errors"   .= map (\(f, e) -> object ["file" .= f, "error" .= e]) arParseErrors
        ]
  in TE.decodeUtf8 $ BL.toStrict $ encodePretty jsonVal

renderParseErrors :: Theme -> [(FilePath, Text)] -> Text
renderParseErrors _ [] = ""
renderParseErrors th errs =
  cRed th "Parse Errors:" <> "\n" <>
  T.intercalate "\n" (map (\(f, e) -> "  " <> T.pack f <> ": " <> e) errs) <>
  "\n\n"

renderFinding :: Theme -> Finding -> Text
renderFinding th Finding{..} =
  let sevIcon = case findingSeverity of
        Error   -> cRed th    "[ERROR]  "
        Warning -> cYellow th "[WARN]   "
        Info    -> cCyan th   "[INFO]   "
      loc = case findingLocation of
        SrcSpan f l c _ _ -> cDim th (T.pack f <> ":" <> T.pack (show l) <> ":" <> T.pack (show c))
      rule = cBlue th (unInspectionId findingInspection)
      context = case (findingTemplate, findingChoice) of
        (Just tpl, Just ch) -> " in " <> cBold th (tpl <> "." <> ch)
        (Just tpl, Nothing) -> " in " <> cBold th tpl
        _                   -> ""
      suggestion = case findingSuggestion of
        Just s  -> "\n           " <> cGreen th "Suggestion: " <> s
        Nothing -> ""
  in sevIcon <> rule <> " " <> loc <> context <> "\n"
  <> "           " <> findingMessage
  <> suggestion <> "\n"

renderSummary :: Theme -> [Finding] -> Text
renderSummary th findings =
  let errors = length $ filter (\f -> findingSeverity f == Error) findings
      warnings = length $ filter (\f -> findingSeverity f == Warning) findings
      infos = length $ filter (\f -> findingSeverity f == Info) findings
  in "\n" <> cDim th "---" <> "\n"
  <> "Summary: "
  <> colorCount th Error errors <> " errors, "
  <> colorCount th Warning warnings <> " warnings, "
  <> colorCount th Info infos <> " info\n"

-- | Colorize a count based on severity (only when nonzero)
colorCount :: Theme -> Severity -> Int -> Text
colorCount _  _       0 = "0"
colorCount th Error   n = cRed th (T.pack (show n))
colorCount th Warning n = cYellow th (T.pack (show n))
colorCount th Info    n = cCyan th (T.pack (show n))
