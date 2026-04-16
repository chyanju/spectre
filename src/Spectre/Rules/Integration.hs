{-# LANGUAGE OverloadedStrings #-}
-- | Integration-related inspection rules.
--
-- Checks for patterns that can cause integration issues when DAML
-- contracts interact with external systems or delegate to other contracts.
module Spectre.Rules.Integration
  ( integrationRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Spectre.Ast
import Spectre.Inspection

-- | All integration rules
integrationRules :: [Inspection]
integrationRules =
  [ uncheckedParamAssignment
  ]

-- | SPEC-INTEG-001: Choice parameter assigned to template field without validation
--
-- STRUCTURAL detection (no naming heuristics):
-- A choice parameter has the SAME TYPE as a template field, and is
-- directly assigned to that field via `create this with { field = param }`,
-- but no assertion in the choice body compares the two values.
--
-- This is a general data-flow check: if you accept user input and directly
-- overwrite contract state with it, you should validate first.
uncheckedParamAssignment :: Inspection
uncheckedParamAssignment = mkInspection
  "SPEC-INTEG-001"
  "Parameter assigned to template field without validation"
  "A choice parameter is assigned directly to a template field of the same type without any assertion comparing them. A caller could pass a mismatched value."
  Warning
  [Invariant]
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INTEG-001") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': parameter '" <> fieldName param
         <> "' is assigned to field '" <> fldName
         <> "' without validation")
        (Just $ "Add 'assertMsg \"" <> fieldName param <> " mismatch\" ("
                <> fieldName param <> " == " <> fldName <> ")' before using the parameter")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , param <- chParams ch
    , let pName = fieldName param
    -- Find template fields that this parameter is assigned to
    , fldName <- findFieldsAssignedParam pName (chBody ch)
    -- Only flag if the param name differs from the field name
    -- (same name = likely intentional shadowing/update)
    , pName /= fldName
    -- Type match: both must have the same type (if type info available)
    , typesMatch (fieldType param) (lookupFieldType fldName (tplFields tpl))
    -- No assertion comparing them
    , not (bodyComparesFields pName fldName (chBody ch))
    ]

-- | Check if two types match (or if type info is unavailable, be permissive)
typesMatch :: Maybe Type -> Maybe Type -> Bool
typesMatch Nothing _ = True  -- no type info → permissive
typesMatch _ Nothing = True
typesMatch (Just (TCon a _)) (Just (TCon b _)) = a == b
typesMatch (Just (TApp a1 a2 _)) (Just (TApp b1 b2 _)) =
  typesMatch (Just a1) (Just b1) && typesMatch (Just a2) (Just b2)
typesMatch _ _ = True  -- complex types → permissive

lookupFieldType :: Text -> [Field] -> Maybe Type
lookupFieldType name fields =
  case [fieldType f | f <- fields, fieldName f == name] of
    (ty:_) -> ty
    []     -> Nothing

-- | Find template field names that a parameter is assigned to
-- in `create this with { fieldName = paramName }` patterns.
findFieldsAssignedParam :: Text -> [Stmt] -> [Text]
findFieldsAssignedParam paramName stmts =
  concatMap checkStmt stmts
  where
    checkStmt (SCreate _ expr _) = checkExpr expr
    checkStmt (SBind _ expr _) = checkCreateExpr expr
    checkStmt (SExpr expr _) = checkCreateExpr expr
    checkStmt _ = []

    checkCreateExpr (EApp (EVar "create" _) arg _) = checkExpr arg
    checkCreateExpr (EApp f a _) = checkCreateExpr f ++ checkCreateExpr a
    checkCreateExpr (EParens e _) = checkCreateExpr e
    checkCreateExpr _ = []

    checkExpr (ERecordUpd _ fields _) = extractAssignedFields fields
    checkExpr (ERecordCon _ fields _) = extractAssignedFields fields
    checkExpr (EApp _ arg _) = checkExpr arg
    checkExpr (EParens e _) = checkExpr e
    checkExpr _ = []

    extractAssignedFields fields =
      [ fName | (fName, EVar v _) <- fields, v == paramName ]

-- | Check if the choice body contains a comparison between two names
-- (in any assertion form, deep search).
bodyComparesFields :: Text -> Text -> [Stmt] -> Bool
bodyComparesFields name1 name2 stmts = any (stmtComparesFields name1 name2) stmts

stmtComparesFields :: Text -> Text -> Stmt -> Bool
stmtComparesFields n1 n2 stmt = case stmt of
  SAssert _ e _ -> exprComparesFields n1 n2 e
  SExpr e _     -> exprComparesFields n1 n2 e
  SBind _ e _   -> exprComparesFields n1 n2 e
  SLet binds _  -> any (\b -> exprComparesFields n1 n2 (bindExpr b)) binds
  _             -> False

exprComparesFields :: Text -> Text -> Expr -> Bool
exprComparesFields n1 n2 expr = case expr of
  EInfix op l r _ | op == "==" || op == "/=" ->
    (mentionsVar n1 l && mentionsVar n2 r)
    || (mentionsVar n2 l && mentionsVar n1 r)
    || exprComparesFields n1 n2 l
    || exprComparesFields n1 n2 r
  EApp f a _   -> exprComparesFields n1 n2 f || exprComparesFields n1 n2 a
  EParens e _  -> exprComparesFields n1 n2 e
  EDo stmts _  -> any (stmtComparesFields n1 n2) stmts
  EIf c t e _  -> exprComparesFields n1 n2 c
    || exprComparesFields n1 n2 t
    || exprComparesFields n1 n2 e
  ELet _ e _   -> exprComparesFields n1 n2 e
  _            -> False

-- | Check if an expression mentions a variable name (possibly dotted)
mentionsVar :: Text -> Expr -> Bool
mentionsVar name (EVar v _) = v == name || T.isSuffixOf ("." <> name) v
mentionsVar name (EFieldAccess e f _) = f == name || mentionsVar name e
mentionsVar name (EApp f a _) = mentionsVar name f || mentionsVar name a
mentionsVar name (EParens e _) = mentionsVar name e
mentionsVar _ _ = False
