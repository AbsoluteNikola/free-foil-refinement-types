{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Language.Sprite.Syntax.Convert.InnerToFTR (convert, convertTerm, ConvertError(..)) where
import Language.Fixpoint.Types.Refinements qualified as T
import qualified Language.Sprite.Syntax.Inner.Abs as I
import qualified Language.Fixpoint.Types.Names as T
import qualified Language.Fixpoint.Horn.Types as H
import Data.Traversable (for)

-- TODO: pretty printer
data ConvertError
  = UnsupportedTerm I.Term
  | HVarArgumentNotVar I.Term
  | FlattedMoreThanOneAnd [I.Term]
  deriving (Show)

convertTerm :: I.Term -> Either ConvertError T.Expr
convertTerm = \case
  I.Var (I.VarIdent varId) -> pure $ T.eVar varId
  I.Boolean b -> pure $ case b of
    I.ConstTrue -> T.PTrue
    I.ConstFalse -> T.PFalse
  I.ConstInt n -> pure $ T.ECon $ T.I n
  I.OpExpr l op r -> do
    l' <- convertTerm l
    r' <- convertTerm r
    pure $ op_ op l' r'
  term -> Left $  UnsupportedTerm term
  where
    op_ = \case
      I.EqOp -> T.PAtom T.Eq
      I.LessOrEqOp -> T.PAtom T.Le
      I.LessOp -> T.PAtom T.Lt
      I.GreaterOrEqOp -> T.PAtom T.Ge
      I.GreaterOp -> T.PAtom T.Gt
      I.AndOp -> \e1 e2 -> T.PAnd [e1, e2]
      I.OrOp -> \e1 e2 -> T.POr [e1, e2]
      I.PlusOp -> T.EBin T.Plus
      I.MinusOp -> T.EBin T.Minus
      I.MultiplyOp -> T.EBin T.Times

flattenAnd :: I.Term -> [I.Term]
flattenAnd = \case
  I.OpExpr l I.AndOp r -> [l, r]
  otherTerm -> [otherTerm]


convert :: I.Term -> Either ConvertError H.Pred
convert (flattenAnd -> terms) = case terms of
  [l, r] -> do
    l' <- convert l
    r' <- convert r
    pure $ H.PAnd [l', r']
  [I.HVar (I.VarIdent hornVarName) args] -> do
    argIds <- for args $ \case
      I.Var (I.VarIdent argId) -> pure $ T.symbol argId
      otherTerm  -> Left $ HVarArgumentNotVar otherTerm
    pure $ H.Var (T.symbol hornVarName) argIds

  [otherTerm] -> do
    pred' <- convertTerm otherTerm
    pure $ H.Reft pred'

  _ -> Left $ FlattedMoreThanOneAnd terms
