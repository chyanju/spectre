{-# LANGUAGE OverloadedStrings #-}
-- | Expression and statement parser for DAML.
module Spectre.Parser.Expr
  ( pExpr
  , pStmt
  , pStmts
  , pPattern
  , pType
  , pPartyExpr
  , pPartyExprs
  , pBinding
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void)
import Data.Char (isUpper)

import Spectre.Ast
import Spectre.Parser.Lexer

-- | Parse a type expression
pType :: Parser Type
pType = do
  t <- pTypeAtom
  -- Check for arrow type
  option t $ do
    _ <- symbol "->"
    t2 <- pType
    return $ TArrow t t2 noSpan

pTypeAtom :: Parser Type
pTypeAtom = choice
  [ TList <$> brackets pType <*> pure noSpan
  , do
      _ <- symbol "("
      ts <- pType `sepBy` comma
      _ <- symbol ")"
      case ts of
        []  -> return $ TTuple [] noSpan   -- unit type ()
        [t] -> return $ TParens t noSpan
        _   -> return $ TTuple ts noSpan
  , do
      name <- pTypeName
      args <- many (try pTypeAtom)
      case args of
        [] -> return $ TCon name noSpan
        _  -> return $ foldl (\acc a -> TApp acc a noSpan) (TCon name noSpan) args
  ]

pTypeName :: Parser Text
pTypeName = lexeme $ do
  c <- upperChar <|> letterChar
  rest <- many (alphaNumChar <|> char '_' <|> char '\'' <|> char '.')
  return $ T.pack (c : rest)

-- | Parse a pattern
pPattern :: Parser Pattern
pPattern = choice
  [ try (PWild noSpan <$ keyword "_")   -- standalone _ (not prefix of _pool)
  , try $ do
      name <- identifier
      _ <- symbol "@"
      pat <- pPatternAtom
      return $ PAs name pat noSpan
  , pPatternAtom
  ]

pPatternAtom :: Parser Pattern
pPatternAtom = choice
  [ try (PWild noSpan <$ keyword "_")   -- standalone _
  , PLit <$> stringLiteral <*> pure noSpan
  , PLit <$> numericLiteral <*> pure noSpan
  , PList <$> brackets (pPattern `sepBy` comma) <*> pure noSpan
  , do
      _ <- symbol "("
      ps <- pPattern `sepBy1` comma
      _ <- symbol ")"
      case ps of
        [p] -> return p  -- parenthesized pattern
        _   -> return $ PTuple ps noSpan
  , try $ do
      con <- pConName
      args <- many (try pPatternAtom)
      case args of
        [] -> return $ PVar con noSpan  -- could be variable or nullary constructor
        _  -> return $ PCon con args noSpan
  , PVar <$> identifier <*> pure noSpan
  ]

pConName :: Parser Text
pConName = lexeme $ do
  c <- upperChar
  rest <- many (alphaNumChar <|> char '_' <|> char '\'' <|> char '.')
  return $ T.pack (c : rest)

-- | Parse a binding (name = expr)
pBinding :: Parser Binding
pBinding = do
  name <- identifier
  _ <- equals
  expr <- pExpr
  return $ Binding name expr noSpan

-- | Parse a record-pattern destructuring binding:
--   ConName { field1, field2, ... } = expr
-- Returns one Binding per destructured field.
pRecordPatternBinding :: Parser [Binding]
pRecordPatternBinding = do
  _ <- pConName
  fields <- braces (identifier `sepBy1` comma)
  _ <- equals
  expr <- pExpr
  return [ Binding f expr noSpan | f <- fields ]

-- | Parse either a single binding or a record-pattern destructuring.
-- Returns a list of Binding in both cases.
pBindingOrRecordPattern :: Parser [Binding]
pBindingOrRecordPattern = try pRecordPatternBinding <|> ((:[]) <$> pBinding)

-- | Parse party expressions (for signatory/observer/controller)
-- Handles: single party, comma-separated parties, and list literals
pPartyExprs :: Parser [PartyExpr]
pPartyExprs = choice
  [ do  -- list literal [p1, p2, ...]
      ps <- brackets (pSinglePartyExpr `sepBy` comma)
      return [PEList ps noSpan]
  , do  -- comma-separated parties: caller, admin
      first <- pSinglePartyExpr
      rest <- many (try $ comma *> pSinglePartyExpr)
      return (first : rest)
  ]

pPartyExpr :: Parser PartyExpr
pPartyExpr = pSinglePartyExpr

pSinglePartyExpr :: Parser PartyExpr
pSinglePartyExpr = choice
  [ try $ do
      -- Dotted field chain: a.b.c or a.b.c arg1 arg2 (e.g., allocation.transferLeg.sender
      -- or Set.toList knownParties, or this.owner)
      obj <- identifier <|> (keyword "this" *> pure "this")
      _ <- char '.' <* sc
      field <- identifier
      -- Parse additional dot-segments: .bar .baz etc.
      moreDots <- many $ try (char '.' <* sc *> identifier)
      let fullDotted = T.intercalate "." (obj : field : moreDots)
      args <- many (try pExprAtom)
      case (moreDots, args) of
        ([], []) -> return $ PEField obj field noSpan     -- simple: this.owner
        (_, [])  -> return $ PEExpr (EVar fullDotted noSpan) noSpan -- multi-dot: a.b.c
        _        -> return $ PEApp fullDotted args noSpan  -- function call with args
  , try $ do
      -- "signatory" used as a function call in party position:
      -- signatory (RecordType with field1; field2; ...)
      _ <- keyword "signatory"
      arg <- pExprAtom
      return $ PEApp "signatory" [arg] noSpan
  , try $ do
      -- Function application: funcName arg1 arg2 (e.g., allocationInstrumentAdmin allocation)
      name <- identifier
      args <- some (try pExprAtom)
      return $ PEApp name args noSpan
  , PEVar <$> identifier <*> pure noSpan
  , PEExpr <$> pExpr <*> pure noSpan
  ]

-- | Parse an expression
pExpr :: Parser Expr
pExpr = pExprOp

-- | Parse infix expressions
pExprOp :: Parser Expr
pExprOp = do
  lhs <- pExprApp
  rest <- many $ try $ do
    scn  -- allow operator on next line (e.g., ensure a > b \n && case ...)
    pos <- getSourcePos
    let col = unPos (sourceColumn pos)
    -- Only accept continuation operators that are indented (col > 1).
    -- Operators at column 1 are new top-level declarations, not continuations.
    if col <= 1
      then fail "infix continuation at column 1 — not a continuation"
      else do
        op <- pInfixOp
        rhs <- pExprApp
        return (op, rhs)
  return $ foldl (\l (op, r) -> EInfix op l r noSpan) lhs rest

pInfixOp :: Parser Text
pInfixOp = choice
  [ symbol "=="
  , symbol "/="
  , symbol "<="
  , symbol ">="
  , symbol "&&"
  , symbol "||"
  , symbol "<>"
  , symbol "++"
  , symbol "<$>"
  , symbol "<*>"
  , try $ symbol "<" <* notFollowedBy (char '-')
  , symbol ">"
  , symbol "+"
  , try $ symbol "-" <* notFollowedBy (char '>')
  , symbol "*"
  , symbol "/"
  , symbol "$"
  , symbol "."
  , symbol "%"
  , try $ do  -- backtick operator
      _ <- char '`'
      name <- some (alphaNumChar <|> char '_' <|> char '.')
      _ <- char '`'
      sc
      return $ T.pack name
  ]

-- | Parse function application (with record construction via 'with')
pExprApp :: Parser Expr
pExprApp = do
  f <- pExprAtom
  -- Check for record construction: TypeName with field1; field2 ...
  mbRec <- optional $ try $ do
    case f of
      EVar name _ | not (T.null name) && isUpperFirst name -> do
        _ <- keyword "with"
        -- Parse the first field (required); record its starting column
        -- so we can reject subsequent lines at lesser indentation.
        _ <- optional (symbol ";" <|> comma)
        scn
        firstPos <- getSourcePos
        let refCol = unPos (sourceColumn firstPos)
        firstField <- pFieldAssignOrCopy
        -- Parse remaining fields, checking indentation
        moreFields <- many $ try $ do
          _ <- optional (symbol ";" <|> comma)
          scn
          pos <- getSourcePos
          let col = unPos (sourceColumn pos)
          -- Only accept fields that start at the same column as the
          -- first field (or deeper).  If we've moved left, this is a
          -- new statement in the enclosing do-block, not a field.
          if col < refCol
            then fail "record field indentation decreased — end of record"
            else pFieldAssignOrCopy
        return $ ERecordCon name (firstField : moreFields) noSpan
      _ -> fail "not a record constructor"
  case mbRec of
    Just rec -> return rec
    Nothing -> do
      args <- many (try pExprAtom)
      case args of
        [] -> return f
        _  -> return $ foldl (\acc a -> EApp acc a noSpan) f args
  where
    isUpperFirst t = case T.uncons t of
      Just (c, _) -> c >= 'A' && c <= 'Z'
      Nothing -> False

-- | Parse atomic expressions
pExprAtom :: Parser Expr
pExprAtom = choice
  [ EString <$> stringLiteral <*> pure noSpan
  , ENum <$> numericLiteral <*> pure noSpan
  , try (EWild noSpan <$ keyword "_")   -- standalone _ (not prefix of _pool)
  , pListExpr
  , pTupleOrParens
  , pIfExpr
  , pCaseExpr
  , pDoExpr
  , pLetExpr
  , pLambdaExpr
  , try pCreateExpr   -- create TypeName with ... as expression
  , try pKeywordExpr   -- DAML keywords usable as expressions (getTime, Some, None, etc.)
  , try pFieldAccess
  , pVarOrCon
  ]

-- | Parse DAML keywords that can appear as expressions.
-- These are keywords rejected by 'identifier' but valid in expression context.
pKeywordExpr :: Parser Expr
pKeywordExpr = choice
  [ EVar <$> keyword "getTime" <*> pure noSpan
  , EVar <$> keyword "Some" <*> pure noSpan
  , EVar <$> keyword "None" <*> pure noSpan
  , EVar <$> keyword "True" <*> pure noSpan
  , EVar <$> keyword "False" <*> pure noSpan
  , EVar <$> keyword "abort" <*> pure noSpan
  , EVar <$> keyword "error" <*> pure noSpan
  , EVar <$> keyword "exerciseByKey" <*> pure noSpan
  , EVar <$> keyword "fetchByKey" <*> pure noSpan
  , EVar <$> keyword "lookupByKey" <*> pure noSpan
  ]

-- | Parse 'create TypeName with ...' as an expression.
-- This handles 'create' when it appears inside an expression rather than
-- at the start of a statement (e.g., toInterfaceContractId <$> create Foo with ...).
-- Returns: EApp (EVar "create") (ERecordCon typeName fields)
-- or:      EApp (EVar "create") (ERecordUpd var fields)
pCreateExpr :: Parser Expr
pCreateExpr = do
  _ <- keyword "create"
  choice
    [ try $ do  -- create TypeName with fields...
        typeName <- pTypeName
        body <- option (ERecordCon typeName [] noSpan) $ do
          _ <- keyword "with"
          _ <- optional (symbol ";" <|> comma)
          scn
          firstPos <- getSourcePos
          let refCol = unPos (sourceColumn firstPos)
          firstField <- pFieldAssignOrCopy
          moreFields <- many $ try $ do
            _ <- optional (symbol ";" <|> comma)
            scn
            pos <- getSourcePos
            let col = unPos (sourceColumn pos)
            if col < refCol
              then fail "record field indentation decreased"
              else pFieldAssignOrCopy
          return $ ERecordCon typeName (firstField : moreFields) noSpan
        return $ EApp (EVar "create" noSpan) body noSpan
    , try $ do  -- create varName with fields... (record update)
        varName <- identifier <|> (keyword "this" *> pure "this")
        _ <- keyword "with"
        _ <- optional (symbol ";" <|> comma)
        scn
        firstPos <- getSourcePos
        let refCol = unPos (sourceColumn firstPos)
        firstField <- pFieldAssignOrCopy
        moreFields <- many $ try $ do
          _ <- optional (symbol ";" <|> comma)
          scn
          pos <- getSourcePos
          let col = unPos (sourceColumn pos)
          if col < refCol
            then fail "record field indentation decreased"
            else pFieldAssignOrCopy
        let body = ERecordUpd (EVar varName noSpan) (firstField : moreFields) noSpan
        return $ EApp (EVar "create" noSpan) body noSpan
    ]

pListExpr :: Parser Expr
pListExpr = EList <$> brackets (pExpr `sepBy` comma) <*> pure noSpan

pTupleOrParens :: Parser Expr
pTupleOrParens = do
  _ <- symbol "("
  es <- pExpr `sepBy` comma
  _ <- symbol ")"
  case es of
    []  -> return $ ETuple [] noSpan    -- unit
    [e] -> return $ EParens e noSpan    -- parenthesized
    _   -> return $ ETuple es noSpan    -- tuple

pIfExpr :: Parser Expr
pIfExpr = do
  _ <- keyword "if"
  cond <- pExpr
  scn  -- allow 'then' on next line (common in do-blocks)
  _ <- keyword "then"
  thenE <- pExpr
  scn  -- allow 'else' on next line
  _ <- keyword "else"
  elseE <- pExpr
  return $ EIf cond thenE elseE noSpan

pCaseExpr :: Parser Expr
pCaseExpr = do
  _ <- keyword "case"
  scrut <- pExpr
  _ <- keyword "of"
  alts <- some $ try $ do
    scn
    pat <- pPattern
    _ <- symbol "->"
    expr <- pExpr
    return (pat, expr)
  return $ ECase scrut alts noSpan

pDoExpr :: Parser Expr
pDoExpr = do
  _ <- keyword "do"
  stmts <- pStmts
  return $ EDo stmts noSpan

pLetExpr :: Parser Expr
pLetExpr = do
  _ <- keyword "let"
  binds <- concat <$> some (try pBindingOrRecordPattern)
  _ <- keyword "in"
  body <- pExpr
  return $ ELet binds body noSpan

pLambdaExpr :: Parser Expr
pLambdaExpr = do
  _ <- symbol "\\"
  pats <- some pPatternAtom
  _ <- symbol "->"
  body <- pExpr
  return $ ELam pats body noSpan

pFieldAccess :: Parser Expr
pFieldAccess = do
  obj <- pVarOrCon
  accesses <- some $ do
    void $ char '.' <* sc
    identifier
  return $ foldl (\e f -> EFieldAccess e f noSpan) obj accesses

pVarOrCon :: Parser Expr
pVarOrCon = lexeme $ do
  c <- letterChar <|> char '_'
  -- Only consume dots for uppercase-initial identifiers (qualified names
  -- like DA.Set.fromList, Set.member).  Lowercase-initial identifiers
  -- (like transfer.requestedAt) must NOT consume dots so that
  -- pFieldAccess can produce proper EFieldAccess nodes.
  rest <- if isUpper c
    then many (alphaNumChar <|> char '_' <|> char '\'' <|> char '.')
    else many (alphaNumChar <|> char '_' <|> char '\'')
  let name = T.pack (c : rest)
  -- Reject structural keywords that should never be consumed as bare
  -- variable names inside an expression.  Value-like keywords (Some,
  -- None, True, False, pure, return, this, …) are intentionally NOT
  -- listed here; they are valid expression atoms.
  if name `Set.member` structuralKeywords
    then fail $ "structural keyword " ++ T.unpack name ++ " cannot be used as expression variable"
    else return $ EVar name noSpan

-- | Keywords that delimit syntactic structure and must never be greedily
-- consumed as variable names by 'pVarOrCon'.  Value-like keywords
-- (Some, None, True, False, pure, return, this, etc.) are excluded so
-- they can be parsed as expressions.
structuralKeywords :: Set Text
structuralKeywords = Set.fromList
  [ "module", "where", "import", "qualified", "as", "hiding"
  , "template", "with", "choice", "controller", "do"
  , "signatory", "observer", "ensure", "key", "maintainer"
  , "nonconsuming", "preconsuming", "postconsuming"
  , "interface", "implements", "requires"
  , "let", "in", "if", "then", "else", "case", "of"
  , "data", "type", "class", "instance", "deriving"
  , "forall"
  ]

-- | Parse statements in a do-block
-- Statements may appear on subsequent lines (indented), so we consume
-- newlines/whitespace (scn) before each statement.
-- We check that the statement starts at column > 1 to avoid consuming
-- top-level declarations as statements.
pStmts :: Parser [Stmt]
pStmts = some $ try $ do
  scn
  pos <- getSourcePos
  let col = unPos (sourceColumn pos)
  if col <= 1
    then fail "statement at column 1 — likely a new top-level declaration"
    else pStmt

-- | Parse a single statement.
-- Guards against accidentally consuming top-level or template-level
-- declarations (template, choice, signatory, etc.) that indicate
-- we've left the do-block.
pStmt :: Parser Stmt
pStmt = do
  notFollowedBy (keyword "template" <|> keyword "module" <|> keyword "import"
                 <|> keyword "data" <|> keyword "type" <|> keyword "class"
                 <|> keyword "instance" <|> keyword "deriving"
                 <|> keyword "choice" <|> keyword "nonconsuming"
                 <|> keyword "preconsuming" <|> keyword "postconsuming"
                 <|> keyword "signatory" <|> keyword "observer"
                 <|> keyword "controller" <|> keyword "key"
                 <|> keyword "ensure" <|> keyword "maintainer"
                 <|> keyword "implements")
  choice
    [ try pCreateStmt
    , try pExerciseStmt
    , try pFetchStmt
    , try pArchiveStmt
    , try pAssertStmt
    , try pReturnStmt
    , try pLetStmt
    , try pBindStmt
    , pExprStmt
    ]

-- | Optional bind pattern: parses "pat <-" prefix if present.
-- Used by action statement parsers to handle both "fetch cid" and "x <- fetch cid".
pOptionalBind :: Parser (Maybe Pattern)
pOptionalBind = optional $ try (pPattern <* symbol "<-")

pCreateStmt :: Parser Stmt
pCreateStmt = do
  mbPat <- pOptionalBind
  _ <- keyword "create"
  choice
    [ try $ do  -- create TypeName [with { field = expr, ... }]
        typeName <- pTypeName
        body <- option (ERecordCon typeName [] noSpan) $ do
          _ <- keyword "with"
          -- Use scn to handle fields on separate lines (newline after 'with')
          fields <- some $ try $ do { _ <- optional (symbol ";" <|> comma); scn; pFieldAssignOrCopy }
          return $ ERecordCon typeName fields noSpan
        return $ case mbPat of
          Nothing  -> SCreate (TCon typeName noSpan) body noSpan
          Just pat -> SBind pat (EApp (EVar "create" noSpan) body noSpan) noSpan
    , try $ do  -- create this with { field = expr, ... } (record update)
        varName <- identifier <|> (keyword "this" *> pure "this")
        _ <- keyword "with"
        -- Use scn to handle fields on separate lines (newline after 'with')
        fields <- some $ try $ do { _ <- optional (symbol ";" <|> comma); scn; pFieldAssignOrCopy }
        let body = ERecordUpd (EVar varName noSpan) fields noSpan
        return $ case mbPat of
          Nothing  -> SCreate (TCon "" noSpan) body noSpan
          Just pat -> SBind pat (EApp (EVar "create" noSpan) body noSpan) noSpan
    ]

-- | Parse a field assignment: either "field = expr" or "field" (shorthand for field = field)
pFieldAssignOrCopy :: Parser (Text, Expr)
pFieldAssignOrCopy = do
  name <- identifier
  choice
    [ do _ <- equals
         val <- pExpr
         return (name, val)
    , return (name, EVar name noSpan)  -- DAML shorthand: field means field = field
    ]

pExerciseStmt :: Parser Stmt
pExerciseStmt = do
  mbPat <- pOptionalBind
  _ <- keyword "exercise"
  cid <- pExprAtom
  choiceName <- pConName
  args <- option (ETuple [] noSpan) pExprAtom
  return $ case mbPat of
    Nothing  -> SExercise cid choiceName args noSpan
    Just pat -> SBind pat (EApp (EVar "exercise" noSpan) cid noSpan) noSpan

pFetchStmt :: Parser Stmt
pFetchStmt = do
  mbPat <- pOptionalBind
  _ <- keyword "fetch"
  cid <- pExprAtom
  return $ case mbPat of
    Nothing  -> SFetch cid noSpan
    Just pat -> SBind pat (EApp (EVar "fetch" noSpan) cid noSpan) noSpan

pArchiveStmt :: Parser Stmt
pArchiveStmt = do
  mbPat <- pOptionalBind
  _ <- keyword "archive"
  cid <- pExprAtom
  return $ case mbPat of
    Nothing  -> SArchive cid noSpan
    Just pat -> SBind pat (EApp (EVar "archive" noSpan) cid noSpan) noSpan

pAssertStmt :: Parser Stmt
pAssertStmt = choice
  [ do
      _ <- keyword "assertMsg"
      msg <- stringLiteral
      cond <- pExpr
      return $ SAssert msg cond noSpan
  , do
      _ <- keyword "assert"
      cond <- pExprAtom
      return $ SAssert "" cond noSpan
  ]

pReturnStmt :: Parser Stmt
pReturnStmt = do
  _ <- keyword "return" <|> keyword "pure"
  val <- pExpr
  return $ SReturn val noSpan

pLetStmt :: Parser Stmt
pLetStmt = do
  _ <- keyword "let"
  binds <- concat <$> some (try pBindingOrRecordPattern)
  return $ SLet binds noSpan

pBindStmt :: Parser Stmt
pBindStmt = do
  pat <- pPattern
  _ <- symbol "<-"
  expr <- pExpr
  return $ SBind pat expr noSpan

pExprStmt :: Parser Stmt
pExprStmt = SExpr <$> pExpr <*> pure noSpan
