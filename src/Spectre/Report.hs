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

-- | Render findings based on output format
renderFindings :: OutputFormat -> AnalysisResult -> Text
renderFindings HumanReadable = renderHuman
renderFindings JsonOutput = renderJson

-- | Render results as human-readable text
renderHuman :: AnalysisResult -> Text
renderHuman AnalysisResult{..} =
  let header = "Spectre DAML Security Analysis\n"
            <> "==============================\n"
            <> "Files analyzed: " <> T.pack (show arFileCount) <> "\n"
            <> "Rules applied: " <> T.pack (show arRulesRun) <> "\n"
            <> "Findings: " <> T.pack (show (length arFindings)) <> "\n"
            <> "\n"
      errors = renderParseErrors arParseErrors
      findings = T.intercalate "\n" (map renderFinding arFindings)
      summary = renderSummary arFindings
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

renderParseErrors :: [(FilePath, Text)] -> Text
renderParseErrors [] = ""
renderParseErrors errs =
  "Parse Errors:\n" <>
  T.intercalate "\n" (map (\(f, e) -> "  " <> T.pack f <> ": " <> e) errs) <>
  "\n\n"

renderFinding :: Finding -> Text
renderFinding Finding{..} =
  let sevIcon = case findingSeverity of
        Error -> "[ERROR]  "
        Warning -> "[WARN]   "
        Info -> "[INFO]   "
      loc = case findingLocation of
        SrcSpan f l c _ _ -> T.pack f <> ":" <> T.pack (show l) <> ":" <> T.pack (show c)
      rule = unInspectionId findingInspection
      context = case (findingTemplate, findingChoice) of
        (Just tpl, Just ch) -> " in " <> tpl <> "." <> ch
        (Just tpl, Nothing) -> " in " <> tpl
        _                   -> ""
      suggestion = case findingSuggestion of
        Just s -> "\n           Suggestion: " <> s
        Nothing -> ""
  in sevIcon <> rule <> " " <> loc <> context <> "\n"
  <> "           " <> findingMessage
  <> suggestion <> "\n"

renderSummary :: [Finding] -> Text
renderSummary findings =
  let errors = length $ filter (\f -> findingSeverity f == Error) findings
      warnings = length $ filter (\f -> findingSeverity f == Warning) findings
      infos = length $ filter (\f -> findingSeverity f == Info) findings
  in "\n---\n"
  <> "Summary: "
  <> T.pack (show errors) <> " errors, "
  <> T.pack (show warnings) <> " warnings, "
  <> T.pack (show infos) <> " info\n"
