{-# LANGUAGE OverloadedStrings #-}
-- | Main parser entry point for DAML files.
module Spectre.Parser
  ( parseDamlFile
  , parseDaml
  , ParseError
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec hiding (ParseError)

import Spectre.Ast
import Spectre.Parser.Lexer
import Spectre.Parser.Template

-- | Parse error type alias
type ParseError = ParseErrorBundle Text Void

-- | Parse a DAML file from disk
parseDamlFile :: FilePath -> IO (Either ParseError Module)
parseDamlFile fp = do
  content <- TIO.readFile fp
  return $ parseDaml fp content

-- | Parse DAML source text
parseDaml :: FilePath -> Text -> Either ParseError Module
parseDaml fp src = parse pModule fp src

-- | Parse a complete DAML module
pModule :: Parser Module
pModule = do
  scn  -- consume leading whitespace/comments
  (fp, sl, sc') <- getPositionFull
  name <- option "Main" $ try pModuleHeader
  scn
  imports <- many $ try (scn *> pImport)
  scn
  decls <- many $ try (scn *> pTopLevelDecl)
  scn
  (_, el, ec) <- getPositionFull
  eof
  return $ Module name imports decls (SrcSpan fp sl sc' el ec)
