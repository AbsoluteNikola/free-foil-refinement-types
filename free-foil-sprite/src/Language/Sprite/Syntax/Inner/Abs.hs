-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Inner.

module Language.Sprite.Syntax.Inner.Abs where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

data Term
    = ConstInt Integer
    | Boolean ConstBool
    | Var VarIdent
    | If Term Term Term
    | Let Pattern Term ScopedTerm
    | LetRec Term Pattern ScopedTerm ScopedTerm
    | Fun Pattern ScopedTerm
    | App Term Term
    | Ann Term Term
    | OpExpr Term Op Term
    | TypeRefined BaseType Pattern ScopedTerm
    | TypeRefinedUnknown BaseType
    | TypeFun Pattern Term ScopedTerm
    | HVar VarIdent [Term]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ConstBool = ConstTrue | ConstFalse
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Op
    = EqOp
    | LessOrEqOp
    | LessOp
    | GreaterOrEqOp
    | GreaterOp
    | PlusOp
    | MinusOp
    | MultiplyOp
    | AndOp
    | OrOp
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Pattern = PatternVar VarIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ScopedTerm = ScopedTerm Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data BaseType = BaseTypeInt | BaseTypeBool
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

newtype VarIdent = VarIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

