-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Syntax.

module Language.Lambda.Syntax.Abs where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

data Term
    = ConstInt Integer
    | Var VarIdent
    | Let Term Pattern ScopedTerm
    | Fun Pattern ScopedTerm
    | App Term Term
    | Ann Term Term
    | TypeRefined BaseType Pattern ScopedTerm
    | TypeFun Term Pattern ScopedTerm
    | PTrue
    | PFalse
    | PLessThan Term Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Pattern = PatternVar VarIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ScopedTerm = ScopedTerm Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data BaseType = BaseTypeInt
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Kind = KindBase | KindStar
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Environment
    = EnvironmentEmpty | EnvironmentNonEmpty [VarBinding]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data VarBinding = VarBinding VarIdent Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

newtype VarIdent = VarIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

