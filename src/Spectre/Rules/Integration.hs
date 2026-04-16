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
import Data.Char (toLower)

import Spectre.Ast
import Spectre.Inspection

-- | All integration rules
integrationRules :: [Inspection]
integrationRules =
  [ uncheckedExpectedParam
  ]

-- | SPEC-INTEG-001: Choice parameter prefixed 'expected' not validated
--
-- When a choice parameter is named 'expected...' (e.g., expectedAdmin,
-- expectedOwner), the intent is to verify the expected value matches
-- the actual template field.  If this comparison is missing, a caller
-- can pass any value, potentially bypassing security checks or
-- redirecting operations.
uncheckedExpectedParam :: Inspection
uncheckedExpectedParam = mkInspection
  "SPEC-INTEG-001"
  "Unchecked 'expected' parameter"
  "A choice parameter prefixed with 'expected' is not validated against the corresponding template field. A caller could pass a mismatched value."
  Warning
  [Invariant]  -- closest existing category
  $ \mod_ ->
    [ mkFinding (InspectionId "SPEC-INTEG-001") Warning (chLocation ch)
        ("Choice '" <> chName ch <> "' in template '" <> tplName tpl
         <> "': parameter '" <> fieldName param
         <> "' is not validated against template field '" <> correspondingField
         <> "'")
        (Just $ "Add 'assertMsg \"" <> fieldName param <> " mismatch\" ("
                <> fieldName param <> " == " <> correspondingField <> ")' before using the parameter")
        (Just (tplName tpl))
        (Just (chName ch))
    | DTemplate tpl <- moduleDecls mod_
    , ch <- tplChoices tpl
    , param <- chParams ch
    , T.isPrefixOf "expected" (fieldName param)
    , let correspondingField = stripExpectedPrefix (fieldName param)
    , not (T.null correspondingField)
    -- Check the corresponding template field exists
    , any (\f -> fieldName f == correspondingField) (tplFields tpl)
    -- Check the choice body does NOT contain an assertion comparing them
    , not (bodyComparesFields (fieldName param) correspondingField (chBody ch))
    ]

-- | Strip the 'expected' prefix and lowercase the first letter of the rest.
-- e.g., "expectedAdmin" -> "admin", "expectedOwner" -> "owner"
stripExpectedPrefix :: Text -> Text
stripExpectedPrefix t =
  let rest = T.drop 8 t  -- drop "expected"
  in if T.null rest
     then ""
     else T.cons (toLower (T.head rest)) (T.tail rest)

-- | Check if the choice body contains a comparison between two field names
-- (in an assert or direct comparison).
bodyComparesFields :: Text -> Text -> [Stmt] -> Bool
bodyComparesFields expected actual stmts = any (stmtComparesFields expected actual) stmts

stmtComparesFields :: Text -> Text -> Stmt -> Bool
stmtComparesFields expected actual stmt = case stmt of
  SAssert _ e _ -> exprComparesFields expected actual e
  SExpr e _ -> exprComparesFields expected actual e
  SBind _ e _ -> exprComparesFields expected actual e
  SLet binds _ -> any (\b -> exprComparesFields expected actual (bindExpr b)) binds
  _ -> False

exprComparesFields :: Text -> Text -> Expr -> Bool
exprComparesFields expected actual expr = case expr of
  -- expected == actual or actual == expected
  EInfix op l r _ | op == "==" || op == "/=" ->
    (mentionsVar expected l && mentionsVar actual r)
    || (mentionsVar actual l && mentionsVar expected r)
    || exprComparesFields expected actual l
    || exprComparesFields expected actual r
  -- assertMsg "msg" (cond)
  EApp f a _ -> exprComparesFields expected actual f || exprComparesFields expected actual a
  EParens e _ -> exprComparesFields expected actual e
  EDo stmts _ -> any (stmtComparesFields expected actual) stmts
  EIf c t e _ -> exprComparesFields expected actual c
    || exprComparesFields expected actual t
    || exprComparesFields expected actual e
  ELet _ e _ -> exprComparesFields expected actual e
  _ -> False

-- | Check if an expression mentions a variable name (possibly dotted)
mentionsVar :: Text -> Expr -> Bool
mentionsVar name (EVar v _) = v == name || T.isSuffixOf ("." <> name) v
mentionsVar name (EFieldAccess e f _) = f == name || mentionsVar name e
mentionsVar name (EApp f a _) = mentionsVar name f || mentionsVar name a
mentionsVar name (EParens e _) = mentionsVar name e
mentionsVar _ _ = False
