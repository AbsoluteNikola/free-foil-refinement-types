module Language.Sprite.Naive.Predicates where

import Language.Sprite.Syntax.Abs (Pred(..), VarIdent (..), RType(..), BaseType(..), FuncAppArg(..), IntOp(..), FuncArg (NamedFuncArg), ScopedRType (ScopedRType))
import Language.Fixpoint.Types.Refinements qualified as T

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
    resultType =
      TypeRefined BaseTypeInt "opRes" $
        PEq (PVar "opRes")
          (opPred
            (PVar leftArgId)
            (PVar rightArgId))

predToFTR :: Pred -> T.Pred
predToFTR = \case
  PVar (VarIdent varId) -> T.eVar varId
  PTrue -> T.PTrue
  PFalse -> T.PFalse
  PInt n -> T.ECon $ T.I n
  PEq lp rp -> toBoolOp T.Eq lp rp
  PLessThan lp rp -> toBoolOp T.Lt lp rp
  PLessOrEqThan lp rp -> toBoolOp T.Le lp rp
  PPlus lp rp -> toIntOp T.Plus lp rp
  PMinus lp rp -> toIntOp T.Minus lp rp
  PMultiply lp rp -> toIntOp T.Times lp rp
  where
    toBoolOp op lp rp = T.PAtom op (predToFTR lp) (predToFTR rp)
    toIntOp op lp rp = T.EBin op (predToFTR lp) (predToFTR rp)
