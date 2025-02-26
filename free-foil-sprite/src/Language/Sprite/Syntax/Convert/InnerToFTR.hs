{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Language.Sprite.Syntax.Convert.InnerToFTR where
import Language.Fixpoint.Types.Refinements qualified as T
import Language.Sprite.Syntax.Inner.Abs as I

-- TODO: pretty printer
data ConvertError = UnsupportedTerm I.Term
  deriving (Show)

convert :: I.Term -> Either ConvertError T.Pred
convert = \case
  I.Var (I.VarIdent varId) -> pure $ T.eVar varId
  I.ConstTrue -> pure T.PTrue
  I.ConstFalse -> pure T.PFalse
  I.ConstInt n -> pure $ T.ECon $ T.I n
  I.OpExpr l op r -> do
    l' <- convert l
    r' <- convert r
    pure $ op_ op l' r'
  term -> Left $  UnsupportedTerm term
  where
    op_ = \case
      I.EqOp -> T.PAtom T.Eq
      I.LessOrEqOp -> T.PAtom T.Le
      I.LessOp -> T.PAtom T.Lt
      I.GreaterOrEqOp -> T.PAtom T.Ge
      I.GreaterOp -> T.PAtom T.Gt
      I.PlusOp -> T.EBin T.Plus
      I.MinusOp -> T.EBin T.Minus
      I.MultiplyOp -> T.EBin T.Times
