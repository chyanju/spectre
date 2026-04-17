{-# LANGUAGE OverloadedStrings #-}
-- | Lexer utilities for DAML parsing.
--
-- Handles whitespace, comments, indentation sensitivity,
-- and DAML-specific keyword recognition.
module Spectre.Parser.Lexer
  ( Parser
  , sc
  , scn
  , lexeme
  , symbol
  , keyword
  , identifier
  , operator
  , stringLiteral
  , numericLiteral
  , parens
  , brackets
  , braces
  , comma
  , colon
  , dot
  , arrow
  , equals
  , pipe
  , backslash
  , underscore
  , indentBlock
  , nonIndented
  , indentGuard
  , getPosition
  , getPositionFull
  , mkSpan
  , damlKeywords
  , damlReservedOps
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Set (Set)
import qualified Data.Set as Set

import Spectre.Ast (SrcSpan(..))

-- | Parser type alias
type Parser = Parsec Void Text

-- | Space consumer (single-line: no newlines)
sc :: Parser ()
sc = L.space hspace1 lineComment blockComment

-- | Space consumer (multi-line: consumes newlines too)
scn :: Parser ()
scn = L.space space1 lineComment blockComment

-- | Line comment
lineComment :: Parser ()
lineComment = L.skipLineComment "--"

-- | Block comment
blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "{-" "-}"

-- | Consume trailing whitespace (single-line)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Match a symbol and consume trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a keyword (must not be followed by identifier char)
keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy (alphaNumChar <|> char '_' <|> char '\''))

-- | Parse an identifier (starts with lowercase or underscore)
identifier :: Parser Text
identifier = lexeme $ do
  c <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  let ident = T.pack (c : rest)
  if ident `Set.member` damlKeywords
    then fail $ "keyword " ++ T.unpack ident ++ " used as identifier"
    else return ident

-- | Parse an operator
operator :: Parser Text
operator = lexeme $ do
  op <- some (oneOf ("!#$%&*+./<=>?@\\^|-~:" :: String))
  let opT = T.pack op
  if opT `Set.member` damlReservedOps
    then fail $ "reserved operator " ++ T.unpack opT
    else return opT

-- | Parse a string literal
stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  _ <- char '"'
  content <- manyTill L.charLiteral (char '"')
  return $ T.pack content

-- | Parse a numeric literal (integer or decimal)
numericLiteral :: Parser Text
numericLiteral = lexeme $ do
  sign <- optional (char '-')
  digits <- some digitChar
  dec <- optional $ do
    d <- char '.'
    ds <- some digitChar
    return (d : ds)
  let num = maybe "" (:[]) sign ++ digits ++ maybe "" id dec
  return $ T.pack num

-- | Parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Square brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Curly braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Comma separator
comma :: Parser Text
comma = symbol ","

-- | Colon (DAML uses single colon for type signatures)
colon :: Parser Text
colon = symbol ":"

-- | Dot
dot :: Parser Text
dot = symbol "."

-- | Arrow
arrow :: Parser Text
arrow = symbol "->"

-- | Equals sign
equals :: Parser Text
equals = symbol "="

-- | Pipe
pipe :: Parser Text
pipe = symbol "|"

-- | Backslash
backslash :: Parser Text
backslash = symbol "\\"

-- | Underscore
underscore :: Parser Text
underscore = symbol "_"

-- | Get current source position
getPosition :: Parser (Int, Int)
getPosition = do
  pos <- getSourcePos
  return (unPos (sourceLine pos), unPos (sourceColumn pos))

-- | Get current source position with file name
getPositionFull :: Parser (FilePath, Int, Int)
getPositionFull = do
  pos <- getSourcePos
  return (sourceName pos, unPos (sourceLine pos), unPos (sourceColumn pos))

-- | Create a SrcSpan from start and end positions
mkSpan :: FilePath -> (Int, Int) -> (Int, Int) -> SrcSpan
mkSpan fp (sl, sc') (el, ec) = SrcSpan fp sl sc' el ec

-- | Indentation-aware block parsing (for templates, choices, etc.)
indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

-- | Non-indented item
nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

-- | Guard on indentation level
indentGuard :: Ordering -> Pos -> Parser Pos
indentGuard = L.indentGuard scn

-- | All DAML keywords
damlKeywords :: Set Text
damlKeywords = Set.fromList
  [ "module", "where", "import", "qualified", "as", "hiding"
  , "template", "with", "choice", "controller", "do"
  , "signatory", "observer", "ensure", "key", "maintainer"
  , "nonconsuming", "preconsuming", "postconsuming"
  , "interface", "implements", "requires"
  , "let", "in", "if", "then", "else", "case", "of"
  , "data", "type", "class", "instance", "deriving"
  , "forall"
  , "True", "False"
  , "create", "exercise", "fetch", "archive"
  , "exerciseByKey", "fetchByKey", "lookupByKey"
  , "return", "pure", "this"
  , "Some", "None", "Optional"
  , "assert", "assertMsg", "abort", "error"
  , "getTime"
  ]

-- | Reserved operators
damlReservedOps :: Set Text
damlReservedOps = Set.fromList
  [ "=", "::", ":", "->", "<-", "\\", "|", "..", "@"
  ]
