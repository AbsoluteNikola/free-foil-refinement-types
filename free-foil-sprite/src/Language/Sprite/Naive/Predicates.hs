module Language.Sprite.Naive.Predicates where

import Language.Sprite.Syntax.Abs (Pred(..), VarIdent (..), RType(..), BaseType(..), FuncAppArg(..), IntOp(..), FuncArg (NamedFuncArg), ScopedRType (ScopedRType))

constIntP :: VarIdent -> Integer -> Pred
constIntP varId x = PEq (PVar varId) (PInt x)

constIntT :: Integer -> RType
constIntT x = TypeRefined BaseTypeInt v $ constIntP v x
  where v = VarIdent "constInt"

funcArgTermToPred :: FuncAppArg -> Pred
funcArgTermToPred = \case
  FuncAppArgInt x -> PInt x
  FuncAppArgVar v -> PVar v

data IntBinOpType = IntBinOpType
  { leftArgId :: VarIdent
  , leftArgT :: RType
  , rightArgId :: VarIdent
  , rightArgT :: RType
  , resultType :: RType
  } deriving (Show)

intBinOpTypes :: IntOp -> IntBinOpType
intBinOpTypes op = IntBinOpType
  { leftArgT = leftArgT
  , leftArgId = leftArgId
  , rightArgId = rightArgId
  , rightArgT = rightArgT
  , resultType = resultType
  }
  where
    leftArgId = "x"
    leftArgT = TypeRefined BaseTypeInt leftArgId PTrue
    rightArgId = "y"
    rightArgT = TypeRefined BaseTypeInt rightArgId PTrue
    opPred = case op of
      IntPlus -> PPlus
      IntMinus -> PMinus
      IntMultiply -> PMultiply
    resultType = TypeFun (NamedFuncArg leftArgId leftArgT) . ScopedRType $
      TypeFun (NamedFuncArg rightArgId rightArgT) . ScopedRType $
        TypeRefined BaseTypeInt "opRes" $
          PEq (PVar "opRes")
            (opPred
              (PVar leftArgId)
              (PVar rightArgId))
