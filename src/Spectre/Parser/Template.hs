{-# LANGUAGE OverloadedStrings #-}
-- | Parser for DAML template and choice declarations.
module Spectre.Parser.Template
  ( pTemplate
  , pChoice
  , pImport
  , pModuleHeader
  , pTopLevelDecl
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (listToMaybe)

import Spectre.Ast
import Spectre.Parser.Lexer
import Spectre.Parser.Expr

-- | Parse a module header: module Foo where
pModuleHeader :: Parser Text
pModuleHeader = do
  _ <- keyword "module"
  name <- pModuleName
  _ <- keyword "where"
  return name

pModuleName :: Parser Text
pModuleName = lexeme $ do
  parts <- pModulePart `sepBy1` char '.'
  return $ T.intercalate "." parts
  where
    pModulePart = do
      c <- upperChar
      rest <- many (alphaNumChar <|> char '_')
      return $ T.pack (c : rest)

-- | Parse an import declaration
pImport :: Parser Import
pImport = do
  _ <- keyword "import"
  qual <- option False (True <$ keyword "qualified")
  name <- pModuleName
  as_ <- optional (keyword "as" *> pModuleName)
  items <- optional $ parens (pImportItem `sepBy` comma)
  return $ Import name qual as_ items noSpan

pImportItem :: Parser Text
pImportItem = lexeme $ do
  c <- letterChar <|> char '_' <|> char '('
  case c of
    '(' -> do
      op <- some (alphaNumChar <|> oneOf ("!#$%&*+./<=>?@\\^|-~:" :: String))
      _ <- char ')'
      return $ T.pack ("(" ++ op ++ ")")
    _ -> do
      rest <- many (alphaNumChar <|> char '_' <|> char '\'' <|> char '.')
      let name = T.pack (c : rest)
      -- Handle (..) for export/import of all constructors
      _ <- optional (string "(..)")
      return name

-- | Parse a top-level declaration
pTopLevelDecl :: Parser Decl
pTopLevelDecl = choice
  [ DTemplate <$> pTemplate
  , try pStandaloneChoice  -- choice snippet without template wrapper
  , try pDataDecl
  , try pTypeSig
  , try pFunctionDecl
  , pOtherDecl
  ]

-- | Parse a standalone choice (without a template wrapper) and wrap it in
-- a synthetic template named "_Snippet".  This handles benchmark files
-- that contain indented choice blocks extracted from a larger template.
pStandaloneChoice :: Parser Decl
pStandaloneChoice = do
  ch <- pChoice
  return $ DTemplate $ Template
    { tplName       = "_Snippet"
    , tplParams     = Nothing
    , tplFields     = []
    , tplSignatory  = []
    , tplObserver   = []
    , tplEnsure     = Nothing
    , tplChoices    = [ch]
    , tplKey        = Nothing
    , tplInterfaces = []
    , tplLocation   = noSpan
    }

-- | Parse a data type declaration
pDataDecl :: Parser Decl
pDataDecl = do
  _ <- keyword "data"
  name <- pConName'
  _ <- optional $ some identifier  -- type parameters
  _ <- equals
  fields <- pDataFields
  return $ DDataType name fields noSpan

pDataFields :: Parser [Field]
pDataFields = choice
  [ -- Record syntax: Foo { field1 : Type, ... }
    do
      _ <- optional pConName'
      braces (pFieldDecl `sepBy` comma)
  , -- Simple constructors: Foo | Bar
    do
      _ <- optional $ pConName' `sepBy1` pipe
      return []
  ]

pFieldDecl :: Parser Field
pFieldDecl = do
  name <- identifier
  _ <- colon <|> symbol "::"
  _ty <- pType
  return $ Field name (Just _ty) noSpan

-- | Parse a type signature
pTypeSig :: Parser Decl
pTypeSig = do
  name <- identifier
  _ <- colon <|> symbol "::"
  ty <- pType
  return $ DTypeSig name ty noSpan

-- | Parse a function declaration
pFunctionDecl :: Parser Decl
pFunctionDecl = do
  name <- identifier
  _ <- many pPatternAtom'  -- function arguments
  _ <- equals
  scn  -- allow body on next line (common in multi-line functions)
  body <- pExpr
  return $ DFunction name Nothing body noSpan

pPatternAtom' :: Parser Pattern
pPatternAtom' = choice
  [ PWild noSpan <$ symbol "_"
  , PVar <$> identifier <*> pure noSpan
  , parens pPattern
  ]

-- | Parse a catch-all "other" declaration (skip to next top-level)
pOtherDecl :: Parser Decl
pOtherDecl = do
  line <- takeWhile1P (Just "declaration") (/= '\n')
  return $ DOther line noSpan

pConName' :: Parser Text
pConName' = lexeme $ do
  c <- upperChar
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  return $ T.pack (c : rest)

-- | Parse a full template declaration
pTemplate :: Parser Template
pTemplate = do
  (fp, sl, sc') <- getPositionFull
  _ <- keyword "template"
  name <- pConName'
  -- Parse template body
  clauses <- many $ try $ scn *> pTemplateClause
  (_, el, ec) <- getPositionFull
  let tpl = buildTemplate name clauses
  return tpl { tplLocation = SrcSpan fp sl sc' el ec }

-- | Template clause types
data TemplateClause
  = TCWith [Field]
  | TCSignatory [PartyExpr]
  | TCObserver [PartyExpr]
  | TCEnsure Expr
  | TCChoice Choice
  | TCKey Key
  | TCWhere
  | TCImplements InterfaceImpl
  | TCOther Text
  deriving (Show)

pTemplateClause :: Parser TemplateClause
pTemplateClause = choice
  [ TCWith <$> pWithBlock
  , TCSignatory <$> (keyword "signatory" *> pPartyExprs)
  , TCObserver <$> (keyword "observer" *> pPartyExprs)
  , TCEnsure <$> (keyword "ensure" *> pExpr)
  , TCChoice <$> pChoice
  , try $ TCKey <$> pKeyDecl
  , TCWhere <$ keyword "where"
  , try $ TCImplements <$> pInterfaceInstance  -- full: interface instance X for Y where ...
  , try $ TCImplements <$> pImplements         -- short: implements X
  ]

-- | Parse a `with` block (record fields)
pWithBlock :: Parser [Field]
pWithBlock = do
  _ <- keyword "with"
  many $ try $ do
    scn
    name <- identifier
    _ <- colon <|> symbol "::"
    ty <- pType
    return $ Field name (Just ty) noSpan

-- | Parse a choice declaration
pChoice :: Parser Choice
pChoice = do
  (fp, sl, sc') <- getPositionFull
  consuming <- pConsuming
  _ <- keyword "choice"
  name <- pConName'
  _ <- colon <|> symbol "::"
  retType <- pType
  clauses <- many $ try $ scn *> pChoiceClause
  (_, el, ec) <- getPositionFull
  let ch = buildChoice name consuming retType clauses
  return ch { chLocation = SrcSpan fp sl sc' el ec }

data ChoiceClause
  = CCWith [Field]
  | CCController [PartyExpr]
  | CCObserver [PartyExpr]
  | CCBody [Stmt]
  | CCOther Text
  deriving (Show)

pChoiceClause :: Parser ChoiceClause
pChoiceClause = choice
  [ CCWith <$> pWithBlock
  , CCController <$> (keyword "controller" *> pPartyExprs)
  , CCObserver <$> (keyword "observer" *> pPartyExprs)
  , CCBody <$> (keyword "do" *> pStmts)
  ]

pConsuming :: Parser Consuming
pConsuming = option Consuming $ choice
  [ NonConsuming <$ keyword "nonconsuming"
  , PreConsuming <$ keyword "preconsuming"
  , PostConsuming <$ keyword "postconsuming"
  ]

pKeyDecl :: Parser Key
pKeyDecl = do
  _ <- keyword "key"
  ty <- pType
  _ <- colon <|> symbol "::"  -- key : Type
  expr <- pExpr
  _ <- keyword "maintainer"
  maint <- pExpr
  return $ Key ty expr maint noSpan

-- | Parse a full interface instance block:
--   interface instance <InterfaceName> for <TemplateName> where
--     view = <expr>
--     methodName args = <expr>
pInterfaceInstance :: Parser InterfaceImpl
pInterfaceInstance = do
  _ <- keyword "interface"
  _ <- keyword "instance"
  ifName <- pModuleName
  _ <- keyword "for"
  _ <- pConName'  -- template name (already known from enclosing template)
  _ <- keyword "where"
  -- Parse indented body: view and method definitions
  items <- many $ try $ do
    scn
    pInterfaceInstanceItem
  let viewExpr = listToMaybe [ e | IIView e <- items ]
      methods  = [ (n, e) | IIMethod n e <- items ]
  return $ InterfaceImpl ifName viewExpr methods noSpan

-- | Items inside an interface instance body
data InterfaceInstanceItem
  = IIView Expr            -- view = <expr>
  | IIMethod Text Expr     -- methodName args = <expr>
  deriving (Show)

-- | Parse a single item inside an interface instance block
pInterfaceInstanceItem :: Parser InterfaceInstanceItem
pInterfaceInstanceItem = choice
  [ try pInterfaceView
  , pInterfaceMethod
  ]

-- | Parse 'view = <expr>' inside an interface instance
pInterfaceView :: Parser InterfaceInstanceItem
pInterfaceView = do
  _ <- keyword "view"
  _ <- equals
  scn  -- allow body on next line
  e <- pExpr
  return $ IIView e

-- | Parse a method implementation: 'methodName args = <expr>'
pInterfaceMethod :: Parser InterfaceInstanceItem
pInterfaceMethod = do
  name <- identifier
  _ <- many pPatternAtom'  -- method arguments (e.g., _self arg)
  _ <- equals
  scn  -- allow body on next line
  body <- pExpr
  return $ IIMethod name body

pImplements :: Parser InterfaceImpl
pImplements = do
  _ <- keyword "implements"
  name <- pModuleName
  return $ InterfaceImpl name Nothing [] noSpan

-- | Build a Template from parsed clauses
buildTemplate :: Text -> [TemplateClause] -> Template
buildTemplate name clauses = Template
  { tplName       = name
  , tplParams     = Nothing
  , tplFields     = concatMap getWith clauses
  , tplSignatory  = concatMap getSig clauses
  , tplObserver   = concatMap getObs clauses
  , tplEnsure     = getFirstEnsure clauses
  , tplChoices    = concatMap getChoice clauses
  , tplKey        = getFirstKey clauses
  , tplInterfaces = concatMap getImpl clauses
  , tplLocation   = noSpan
  }
  where
    getWith (TCWith fs) = fs
    getWith _ = []
    getSig (TCSignatory ps) = ps
    getSig _ = []
    getObs (TCObserver ps) = ps
    getObs _ = []
    getFirstEnsure [] = Nothing
    getFirstEnsure (TCEnsure e : _) = Just e
    getFirstEnsure (_ : rest) = getFirstEnsure rest
    getChoice (TCChoice c) = [c]
    getChoice _ = []
    getFirstKey [] = Nothing
    getFirstKey (TCKey k : _) = Just k
    getFirstKey (_ : rest) = getFirstKey rest
    getImpl (TCImplements i) = [i]
    getImpl _ = []

-- | Build a Choice from parsed clauses
buildChoice :: Text -> Consuming -> Type -> [ChoiceClause] -> Choice
buildChoice name consuming retType clauses = Choice
  { chName       = name
  , chConsuming  = consuming
  , chReturnType = Just retType
  , chParams     = concatMap getWith clauses
  , chController = concatMap getCtrl clauses
  , chObserver   = getFirstObs clauses
  , chBody       = concatMap getBody clauses
  , chLocation   = noSpan
  }
  where
    getWith (CCWith fs) = fs
    getWith _ = []
    getCtrl (CCController ps) = ps
    getCtrl _ = []
    getFirstObs [] = Nothing
    getFirstObs (CCObserver ps : _) = Just ps
    getFirstObs (_ : rest) = getFirstObs rest
    getBody (CCBody stmts) = stmts
    getBody _ = []
