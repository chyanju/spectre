{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Core AST types for DAML source code.
--
-- This AST is designed specifically for security analysis, making
-- DAML-specific constructs (template, choice, signatory, observer, etc.)
-- first-class nodes rather than burying them in generic Haskell AST.
module Spectre.Ast
  ( Module(..)
  , Import(..)
  , Decl(..)
  , Template(..)
  , Choice(..)
  , Consuming(..)
  , Field(..)
  , Key(..)
  , InterfaceImpl(..)
  , Stmt(..)
  , Expr(..)
  , Pattern(..)
  , Binding(..)
  , Type(..)
  , PartyExpr(..)
  , SrcSpan(..)
  , noSpan
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON, object, (.=))

-- | Source location information
data SrcSpan = SrcSpan
  { spanFile   :: !FilePath
  , spanStartLine :: !Int
  , spanStartCol  :: !Int
  , spanEndLine   :: !Int
  , spanEndCol    :: !Int
  } deriving (Show, Eq, Ord, Generic, FromJSON)

instance ToJSON SrcSpan where
  toJSON (SrcSpan f sl sc' el ec) = object
    [ "file"       .= f
    , "start_line" .= sl
    , "start_col"  .= sc'
    , "end_line"   .= el
    , "end_col"    .= ec
    ]

-- | A dummy span for synthesized nodes
noSpan :: SrcSpan
noSpan = SrcSpan "<no-location>" 0 0 0 0

-- | A DAML module
data Module = Module
  { moduleName    :: !Text
  , moduleImports :: ![Import]
  , moduleDecls   :: ![Decl]
  , moduleSpan    :: !SrcSpan
  } deriving (Show, Eq, Generic)

-- | An import declaration
data Import = Import
  { importModule   :: !Text
  , importQualified :: !Bool
  , importAs       :: !(Maybe Text)
  , importItems    :: !(Maybe [Text])  -- Nothing = import all
  , importSpan     :: !SrcSpan
  } deriving (Show, Eq, Generic)

-- | Top-level declarations
data Decl
  = DTemplate !Template
  | DDataType !Text ![Field] !SrcSpan         -- data Foo = Foo { ... }
  | DFunction !Text !(Maybe Type) !Expr !SrcSpan  -- foo : Type \n foo = expr
  | DTypeSig !Text !Type !SrcSpan             -- foo : Type
  | DClass !Text !SrcSpan                     -- class declaration (stub)
  | DInstance !Text !SrcSpan                   -- instance declaration (stub)
  | DOther !Text !SrcSpan                     -- anything we don't parse deeply
  deriving (Show, Eq, Generic)

-- | A DAML template — the core security-relevant construct
data Template = Template
  { tplName       :: !Text
  , tplParams     :: !(Maybe Text)          -- template parameter variable name
  , tplFields     :: ![Field]               -- the `with` block
  , tplSignatory  :: ![PartyExpr]           -- signatory declarations
  , tplObserver   :: ![PartyExpr]           -- observer declarations
  , tplEnsure     :: !(Maybe Expr)          -- ensure clause
  , tplChoices    :: ![Choice]              -- choice declarations
  , tplKey        :: !(Maybe Key)           -- key declaration
  , tplInterfaces :: ![InterfaceImpl]       -- interface implementations
  , tplLocation   :: !SrcSpan
  } deriving (Show, Eq, Generic)

-- | Choice consuming semantics
data Consuming
  = Consuming        -- default: archives the contract
  | NonConsuming     -- nonconsuming: contract stays active
  | PreConsuming     -- preconsuming: archive before body
  | PostConsuming    -- postconsuming: archive after body
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | A choice within a template
data Choice = Choice
  { chName       :: !Text
  , chConsuming  :: !Consuming
  , chReturnType :: !(Maybe Type)
  , chParams     :: ![Field]               -- the `with` block
  , chController :: ![PartyExpr]           -- controller declaration
  , chObserver   :: !(Maybe [PartyExpr])   -- observer on choice
  , chBody       :: ![Stmt]                -- do-block statements
  , chLocation   :: !SrcSpan
  } deriving (Show, Eq, Generic)

-- | A record field (used in template `with` and choice `with`)
data Field = Field
  { fieldName :: !Text
  , fieldType :: !(Maybe Type)
  , fieldSpan :: !SrcSpan
  } deriving (Show, Eq, Generic)

-- | Template key declaration
data Key = Key
  { keyType       :: !Type
  , keyExpr       :: !Expr
  , keyMaintainer :: !Expr
  , keySpan       :: !SrcSpan
  } deriving (Show, Eq, Generic)

-- | Interface implementation within a template
data InterfaceImpl = InterfaceImpl
  { ifaceName    :: !Text
  , ifaceViewExpr :: !(Maybe Expr)            -- view = <expr>
  , ifaceMethods :: ![(Text, Expr)]           -- method implementations: name -> body
  , ifaceSpan    :: !SrcSpan
  } deriving (Show, Eq, Generic)

-- | Statements in a do-block
data Stmt
  = SBind !Pattern !Expr !SrcSpan           -- pat <- expr
  | SLet ![Binding] !SrcSpan                -- let bindings
  | SCreate !Type !Expr !SrcSpan            -- create Foo with ...
  | SExercise !Expr !Text !Expr !SrcSpan    -- exercise cid ChoiceName arg
  | SExerciseByKey !Type !Expr !Text !Expr !SrcSpan
  | SFetch !Expr !SrcSpan                   -- fetch cid
  | SArchive !Expr !SrcSpan                 -- archive cid
  | SAssert !Text !Expr !SrcSpan            -- assertMsg "msg" cond
  | SReturn !Expr !SrcSpan                  -- return / pure
  | SExpr !Expr !SrcSpan                    -- any other expression statement
  deriving (Show, Eq, Generic)

-- | Expressions (simplified for analysis purposes)
data Expr
  = EVar !Text !SrcSpan                       -- variable reference
  | ELit !Text !SrcSpan                       -- literal (string, number, etc.)
  | EApp !Expr !Expr !SrcSpan                 -- function application
  | EInfix !Text !Expr !Expr !SrcSpan         -- infix operator: a `op` b
  | ELam ![Pattern] !Expr !SrcSpan            -- lambda: \x -> e
  | ELet ![Binding] !Expr !SrcSpan            -- let ... in ...
  | EIf !Expr !Expr !Expr !SrcSpan            -- if c then t else f
  | ECase !Expr ![(Pattern, Expr)] !SrcSpan   -- case expr of ...
  | EDo ![Stmt] !SrcSpan                      -- do { ... }
  | EList ![Expr] !SrcSpan                    -- [a, b, c]
  | ETuple ![Expr] !SrcSpan                   -- (a, b)
  | ERecordCon !Text ![(Text, Expr)] !SrcSpan -- Foo { x = 1, y = 2 }
  | ERecordUpd !Expr ![(Text, Expr)] !SrcSpan -- foo { x = 1 }
  | EFieldAccess !Expr !Text !SrcSpan         -- expr.field
  | ENeg !Expr !SrcSpan                       -- negation
  | EParens !Expr !SrcSpan                    -- (expr)
  | ETypeSig !Expr !Type !SrcSpan             -- expr : Type
  | EWild !SrcSpan                            -- _
  | EString !Text !SrcSpan                    -- "string literal"
  | ENum !Text !SrcSpan                       -- numeric literal
  deriving (Show, Eq, Generic)

-- | Patterns in bindings
data Pattern
  = PVar !Text !SrcSpan
  | PWild !SrcSpan
  | PCon !Text ![Pattern] !SrcSpan       -- Constructor pat1 pat2
  | PTuple ![Pattern] !SrcSpan           -- (p1, p2)
  | PLit !Text !SrcSpan
  | PList ![Pattern] !SrcSpan
  | PAs !Text !Pattern !SrcSpan          -- x@pat
  deriving (Show, Eq, Generic)

-- | Let/where bindings
data Binding = Binding
  { bindName :: !Text
  , bindExpr :: !Expr
  , bindSpan :: !SrcSpan
  } deriving (Show, Eq, Generic)

-- | Type expressions
data Type
  = TCon !Text !SrcSpan                  -- named type
  | TApp !Type !Type !SrcSpan            -- type application
  | TArrow !Type !Type !SrcSpan          -- a -> b
  | TTuple ![Type] !SrcSpan              -- (a, b)
  | TList !Type !SrcSpan                 -- [a]
  | TParens !Type !SrcSpan              -- (a)
  | TOptional !Type !SrcSpan             -- Optional a
  deriving (Show, Eq, Generic)

-- | Party expressions (used in signatory/observer/controller)
data PartyExpr
  = PEVar !Text !SrcSpan                 -- simple variable: owner
  | PEField !Text !Text !SrcSpan         -- field access: this.owner
  | PEList ![PartyExpr] !SrcSpan         -- [p1, p2]
  | PEApp !Text ![Expr] !SrcSpan         -- function call: signatory (getParties this)
  | PEExpr !Expr !SrcSpan                -- arbitrary expression
  deriving (Show, Eq, Generic)
