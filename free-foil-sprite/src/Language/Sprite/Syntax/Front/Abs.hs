-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Front.

module Language.Sprite.Syntax.Front.Abs where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

data Program = Program [Qualifier] [Measure] [DataType] Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Qualifier = Qualifier VarIdent [QualifierArg] Pred
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data QualifierArg = QualifierArg VarIdent RType
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Measure = Measure VarIdent RType
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data MeasureIdent
    = MeasureIdAsVar VarIdent | MeasureIdAsCon ConIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Term
    = ConstInt Integer
    | Bool ConstBool
    | Var VarIdent
    | ConApp ConIdent ConAppArgs
    | FunApp VarIdent [FuncAppArg]
    | If FuncAppArg Term Term
    | Let Decl Term
    | Fun [FunArgName] Term
    | Op FuncAppArg IntOp FuncAppArg
    | Switch VarIdent [SwitchCase]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ConstBool = ConstTrue | ConstFalse
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data ConAppArgs = EmptyConAppArgs | NonEmptyConAppArgs [FuncAppArg]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Annotation = Annotation VarIdent RType
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Decl
    = RecDecl Annotation VarIdent Term
    | AnnotatedDecl Annotation VarIdent Term
    | UnAnnotatedDecl VarIdent Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data SwitchCase = SwitchCase ConIdent SwitchCaseDataConArgs Term
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data SwitchCaseDataConArgs
    = SwitchCaseNonEmptyDataConArgs [FunArgName]
    | SwitchCaseEmptyDataConArgs
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data IntOp
    = IntPlus
    | IntMinus
    | IntMultiply
    | IntEq
    | IntLessThan
    | IntLessOrEqThan
    | IntGreaterThan
    | IntGreaterOrEqThan
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Refinement
    = KnownRefinement VarIdent Pred
    | UnknownRefinement
    | SimpleRefinement
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data RType
    = TypeFun FuncArg RType
    | TypeRefined BaseType Refinement
    | TypeData VarIdent TypeDataArgs Refinement
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data TypeDataArg = TypeDataArg RType
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data TypeDataArgs
    = NonEmptyTypeDataArgs [TypeDataArg] | EmptyTypeDataArgs
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data DataType
    = DataType VarIdent DataTypeArgs [DataTypeConstructor]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data DataTypeConstructor
    = DataTypeConstructor ConIdent DataTypeConstructorArgs DataTypeConstructorPredicate
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data DataTypeConstructorArgs
    = NonEmptyDataTypeConstructorArgs [FuncArg]
    | EmptyDataTypeConstructorArgs
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data DataTypeConstructorPredicate
    = NonEmptyDataTypeConstructorPredicate VarIdent Pred
    | EmptyDataTypeConstructorPredicate
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data DataTypeArgs
    = NonEmptyDataTypeArgs [TypeVarId] | EmptyDataTypeArgs
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data FuncArg = NamedFuncArg VarIdent RType | UnNamedFuncArg RType
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Pred
    = PVar VarIdent
    | PBool ConstBool
    | PInt Integer
    | PMeasure MeasureIdent [MeasureArg]
    | POr Pred Pred
    | PAnd Pred Pred
    | PEq Pred Pred
    | PLessThan Pred Pred
    | PLessOrEqThan Pred Pred
    | PGreaterThan Pred Pred
    | PGreaterOrEqThan Pred Pred
    | PPlus Pred Pred
    | PMinus Pred Pred
    | PMultiply Pred Pred
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data MeasureArg = MeasureArg Pred
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data BaseType = BaseTypeInt | BaseTypeBool | BaseTypeVar TypeVarId
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data TypeVarId = TypeVarId VarIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data FunArgName = FunArgName VarIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data FuncAppArg
    = FuncAppArgBool ConstBool
    | FuncAppArgInt Integer
    | FuncAppArgVar VarIdent
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

newtype VarIdent = VarIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

newtype ConIdent = ConIdent String
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic, Data.String.IsString)

