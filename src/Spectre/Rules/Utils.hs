{-# LANGUAGE OverloadedStrings #-}
-- | Shared utilities for inspection rules.
--
-- Provides universal AST traversal, assertion detection, and
-- type-based field classification — so that individual rule modules
-- do not re-implement partial traversals or hardcode magic strings.
module Spectre.Rules.Utils
  ( -- * Universal AST fold / traversal
    foldExpr
  , foldStmts
  , anyExpr
  , anyStmt
    -- * Universal assertion detection
  , isAssertionStmt
  , stmtAssertsCond
  , bodyHasAssertionMentioning
    -- * Type-based field helpers
  , isTimeType
  , isTimeField
  , isCollectionField
  , isCollectionType
  , isPartySetField
  , isNumericType
  , isNumericField
    -- * Collection function detection (stdlib-aware)
  , isCollectionInsertCall
  , isCollectionDeleteCall
  , isCollectionMemberCall
    -- * Misc expression helpers
  , exprMentionsAny
  , exprMentionsName
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Spectre.Ast

-- =========================================================================
-- Universal AST fold / traversal
-- =========================================================================

-- | Recursively fold over all sub-expressions, collecting results.
-- This handles ALL Expr constructors so callers never miss nested code.
foldExpr :: (Expr -> [a]) -> Expr -> [a]
foldExpr f expr = f expr ++ children
  where
    children = case expr of
      EVar _ _            -> []
      ELit _ _            -> []
      EString _ _         -> []
      ENum _ _            -> []
      EWild _             -> []
      EApp a b _          -> foldExpr f a ++ foldExpr f b
      EInfix _ l r _      -> foldExpr f l ++ foldExpr f r
      ELam _ body _       -> foldExpr f body
      ELet binds body _   -> concatMap (foldExpr f . bindExpr) binds ++ foldExpr f body
      EIf c t e _         -> foldExpr f c ++ foldExpr f t ++ foldExpr f e
      ECase scrut alts _  -> foldExpr f scrut ++ concatMap (foldExpr f . snd) alts
      EDo stmts _         -> foldStmts f stmts
      EList es _          -> concatMap (foldExpr f) es
      ETuple es _         -> concatMap (foldExpr f) es
      ERecordCon _ flds _ -> concatMap (foldExpr f . snd) flds
      ERecordUpd e flds _ -> foldExpr f e ++ concatMap (foldExpr f . snd) flds
      EFieldAccess e _ _  -> foldExpr f e
      ENeg e _            -> foldExpr f e
      EParens e _         -> foldExpr f e
      ETypeSig e _ _      -> foldExpr f e

-- | Recursively fold over all expressions within a statement list.
foldStmts :: (Expr -> [a]) -> [Stmt] -> [a]
foldStmts f = concatMap go
  where
    go (SBind _ e _)            = foldExpr f e
    go (SLet binds _)           = concatMap (foldExpr f . bindExpr) binds
    go (SCreate _ e _)          = foldExpr f e
    go (SExercise e _ a _)      = foldExpr f e ++ foldExpr f a
    go (SExerciseByKey _ e _ a _) = foldExpr f e ++ foldExpr f a
    go (SFetch e _)             = foldExpr f e
    go (SArchive e _)           = foldExpr f e
    go (SAssert _ e _)          = foldExpr f e
    go (SReturn e _)            = foldExpr f e
    go (SExpr e _)              = foldExpr f e

-- | Check if any sub-expression satisfies a predicate.
anyExpr :: (Expr -> Bool) -> Expr -> Bool
anyExpr p = not . null . foldExpr (\e -> [() | p e])

-- | Check if any expression within a statement list satisfies a predicate.
anyStmt :: (Expr -> Bool) -> [Stmt] -> Bool
anyStmt p = not . null . foldStmts (\e -> [() | p e])

-- =========================================================================
-- Universal assertion detection
-- =========================================================================

-- | Is a statement an assertion of any form?
-- Recognizes:
--   * SAssert (parser-level assertMsg)
--   * assertMsg "msg" cond  (as expression)
--   * assert cond
--   * when (not cond) (abort ...)  /  unless cond (abort ...)
--   * require "msg" cond  (DA.Assert)
isAssertionStmt :: Stmt -> Bool
isAssertionStmt (SAssert _ _ _) = True
isAssertionStmt (SExpr e _)     = isAssertionExpr e
isAssertionStmt _               = False

isAssertionExpr :: Expr -> Bool
isAssertionExpr (EApp (EApp (EVar fn _) _ _) _ _)
  | fn `elem` ["assertMsg", "require"] = True
isAssertionExpr (EApp (EVar fn _) _ _)
  | fn `elem` ["assert", "abort", "error"] = True
-- when (not cond) (abort ...) or unless cond (abort ...)
isAssertionExpr (EApp (EApp (EVar fn _) _cond _) body _)
  | fn `elem` ["when", "unless"] = anyExpr isAbortLike body
isAssertionExpr _ = False

isAbortLike :: Expr -> Bool
isAbortLike (EVar fn _) = fn `elem` ["abort", "error", "fail"]
isAbortLike (EApp (EVar fn _) _ _) = fn `elem` ["abort", "error", "fail"]
isAbortLike _ = False

-- | Does a statement assert something about a condition expression?
-- Returns True if the statement is assertion-like AND the condition
-- part satisfies the given predicate.
stmtAssertsCond :: (Expr -> Bool) -> Stmt -> Bool
stmtAssertsCond p (SAssert _ cond _) = p cond
stmtAssertsCond p (SExpr e _) = exprAssertsCond p e
stmtAssertsCond _ _ = False

exprAssertsCond :: (Expr -> Bool) -> Expr -> Bool
exprAssertsCond p (EApp (EApp (EVar fn _) _msg _) cond _)
  | fn `elem` ["assertMsg", "require"] = p cond
exprAssertsCond p (EApp (EVar "assert" _) cond _) = p cond
-- when (not cond) ... → the condition is inside the `not`
exprAssertsCond p (EApp (EApp (EVar "when" _) (EApp (EVar "not" _) cond _) _) _ _) = p cond
-- unless cond ...
exprAssertsCond p (EApp (EApp (EVar "unless" _) cond _) _ _) = p cond
exprAssertsCond _ _ = False

-- | Does the choice body have any assertion-like statement whose
-- condition expression mentions ANY of the given names?
bodyHasAssertionMentioning :: [Text] -> [Stmt] -> Bool
bodyHasAssertionMentioning names stmts =
  any (stmtAssertsCond (exprMentionsAny names)) stmts

-- =========================================================================
-- Type-based field classification
-- =========================================================================

-- | Check if a type is Time or RelTime
isTimeType :: Type -> Bool
isTimeType (TCon name _) = name `elem` ["Time", "RelTime", "DA.Time.Time"]
isTimeType _ = False

-- | Check if a field has Time type.
-- Falls back to a MINIMAL name heuristic ONLY if type info is missing.
isTimeField :: Field -> Bool
isTimeField f = case fieldType f of
  Just ty -> isTimeType ty
  Nothing -> isTimeFieldNameFallback (fieldName f)

-- | Minimal fallback: only the most unambiguous time-related substrings.
-- This is ONLY used when the parser could not extract type information.
isTimeFieldNameFallback :: Text -> Bool
isTimeFieldNameFallback name =
  let lower = T.toLower name
  in any (`T.isInfixOf` lower) ["deadline", "expir", "timeout"]

-- | Check if a type is a collection (List, Set, Map)
isCollectionType :: Type -> Bool
isCollectionType (TList _ _) = True
isCollectionType (TApp (TCon name _) _ _) = isCollectionTypeName name
isCollectionType (TCon name _) = isCollectionTypeName name
isCollectionType _ = False

isCollectionTypeName :: Text -> Bool
isCollectionTypeName name =
  -- Suffixes: after stripping module qualifiers, the type is Set or Map
  let base = snd $ T.breakOnEnd "." name  -- "DA.Set" → "Set", "Set" → "Set"
      base' = if T.null base then name else base
  in base' `elem` ["Set", "Map"]

-- | Check if a field has a collection type
isCollectionField :: Field -> Bool
isCollectionField f = case fieldType f of
  Just ty -> isCollectionType ty
  Nothing -> False

-- | Check if a field is a Set Party or [Party] (for role-membership detection)
isPartySetField :: Field -> Bool
isPartySetField f = case fieldType f of
  Just (TApp (TCon name _) (TCon "Party" _) _)
    | isCollectionTypeName name -> True
  Just (TList (TCon "Party" _) _) -> True
  _ -> False

-- | Check if a type is numeric
isNumericType :: Type -> Bool
isNumericType (TCon name _) =
  let base = snd $ T.breakOnEnd "." name
      base' = if T.null base then name else base
  in base' `elem` ["Int", "Decimal", "Numeric", "Integer", "Natural"]
isNumericType (TApp (TCon "Numeric" _) _ _) = True  -- Numeric n
isNumericType _ = False

-- | Check if a field has a numeric type.
-- NO name-based fallback — if we don't have type info, we don't guess.
isNumericField :: Field -> Bool
isNumericField f = case fieldType f of
  Just ty -> isNumericType ty
  Nothing -> False

-- =========================================================================
-- Collection function detection (stdlib-aware)
-- =========================================================================

-- | Is this a call to a collection insert function?
-- Matches: Set.insert, Map.insert, DA.Set.insert, insert, etc.
isCollectionInsertCall :: Text -> Bool
isCollectionInsertCall name =
  let base = snd $ T.breakOnEnd "." name
      base' = if T.null base then name else base
  in base' == "insert"

-- | Is this a call to a collection delete function?
isCollectionDeleteCall :: Text -> Bool
isCollectionDeleteCall name =
  let base = snd $ T.breakOnEnd "." name
      base' = if T.null base then name else base
  in base' `elem` ["delete", "filter"]

-- | Is this a call to a collection membership function?
isCollectionMemberCall :: Text -> Bool
isCollectionMemberCall name =
  let base = snd $ T.breakOnEnd "." name
      base' = if T.null base then name else base
  in base' `elem` ["member", "lookup", "elem", "notMember", "notElem"]

-- =========================================================================
-- Expression helpers
-- =========================================================================

-- | Check if an expression mentions ANY of the given names (deep search).
exprMentionsAny :: [Text] -> Expr -> Bool
exprMentionsAny names = anyExpr isMatch
  where
    isMatch (EVar v _) = v `elem` names || any (\n -> ("." <> n) `T.isSuffixOf` v) names
    isMatch (EFieldAccess _ field _) = field `elem` names
    isMatch _ = False

-- | Check if an expression mentions a specific name (deep search).
exprMentionsName :: Text -> Expr -> Bool
exprMentionsName name = exprMentionsAny [name]
