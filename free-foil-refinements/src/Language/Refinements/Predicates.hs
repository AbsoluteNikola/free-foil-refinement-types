module Language.Refinements.Predicates (ConvertError, convertPredicate, convertExpr) where
import qualified Language.Refinements.Predicates.Abs as P
import Data.Traversable (for)
import qualified Language.Fixpoint.Types.Spans as LF
import qualified Language.Fixpoint.Types.Names as LF
import qualified Language.Fixpoint.Types.Refinements as LF
import qualified Language.Fixpoint.Horn.Types as LFH


data ConvertError
  = HornVarNotOnTopOfPredicate String
  | HVarArgumentNotVar String P.Pred
  | FlattedMoreThanOneAnd [P.Pred]
  deriving (Show)

convertExpr :: P.Pred -> Either ConvertError LF.Expr
convertExpr = \case
  P.Var (P.Id varId) -> pure $ LF.eVar varId
  P.Boolean b -> pure $ case b of
    P.ConstTrue -> LF.PTrue
    P.ConstFalse -> LF.PFalse
  P.ConstInt n -> pure $ LF.ECon $ LF.I n
  P.OpExpr l op r -> do
    l' <- convertExpr l
    r' <- convertExpr r
    pure $ op_ op l' r'
  P.MeasureCall (P.Id measureName) args -> do
    args' <- for args convertExpr
    pure $ LF.mkEApp (LF.dummyLoc $ LF.symbol measureName) args'
  P.HVar (P.Id name) _ -> Left $ HornVarNotOnTopOfPredicate name
  where
    op_ = \case
      P.EqOp -> LF.PAtom LF.Eq
      P.LessOrEqOp -> LF.PAtom LF.Le
      P.LessOp -> LF.PAtom LF.Lt
      P.GreaterOrEqOp -> LF.PAtom LF.Ge
      P.GreaterOp -> LF.PAtom LF.Gt
      P.AndOp -> \e1 e2 -> LF.PAnd [e1, e2]
      P.OrOp -> \e1 e2 -> LF.POr [e1, e2]
      P.PlusOp -> LF.EBin LF.Plus
      P.MinusOp -> LF.EBin LF.Minus
      P.MultiplyOp -> LF.EBin LF.Times


flattenAnd :: P.Pred -> [P.Pred]
flattenAnd = \case
  P.OpExpr l P.AndOp r -> [l, r]
  otherTerm -> [otherTerm]


convertPredicate :: P.Pred -> Either ConvertError LFH.Pred
convertPredicate (flattenAnd -> terms) = case terms of
  [l, r] -> do
    l' <- convertPredicate l
    r' <- convertPredicate r
    pure $ LFH.PAnd [l', r']
  [P.HVar (P.Id hornVarName) args] -> do
    argIds <- for args $ \case
      P.Var (P.Id argId) -> pure $ LF.symbol argId
      otherTerm  -> Left $ HVarArgumentNotVar hornVarName otherTerm
    pure $ LFH.Var (LF.symbol hornVarName) argIds

  [otherTerm] -> do
    pred' <- convertExpr otherTerm
    pure $ LFH.Reft pred'

  _ -> Left $ FlattedMoreThanOneAnd terms
