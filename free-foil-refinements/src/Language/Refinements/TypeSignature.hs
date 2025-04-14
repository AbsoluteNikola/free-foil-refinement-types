module Language.Refinements.TypeSignature
  ( P.Type(..)
  , P.Id(..)
  , P.DataTypeArg(..)
  , P.Pred(..)
  , P.Op(..)
  , P.ConstBool(..)
  , typeToSort
  ) where

import qualified Language.Refinements.Predicates.Abs as P
import qualified Language.Fixpoint.Types.Sorts as LF
import qualified Language.Fixpoint.Types.Names as LF
import qualified Language.Fixpoint.Types.Spans as LF
import Data.Foldable (foldl')
import Data.List (nub)

typeToSort :: P.Type -> LF.Sort
typeToSort typ = generalize (go typ)
  where
    generalize s =
      let
        freeVars = collectFreeVars typ
        subst = LF.mkSortSubst $ zip freeVars (LF.FVar <$> [0..])
      in foldl' (flip LF.FAbs) (LF.sortSubst subst s) [0 .. length freeVars - 1]
    go = \case
      P.BoolType -> LF.boolSort
      P.IntType -> LF.intSort
      P.VarType (P.Id varName) -> LF.fObj (LF.dummyLoc $ LF.symbol varName)
      P.DataType{} -> undefined
      P.FunType argTyp retTyp ->
        LF.FFunc (typeToSort argTyp) (typeToSort retTyp)

collectFreeVars :: P.Type -> [LF.Symbol]
collectFreeVars = nub . go
  where
    go = \case
      P.VarType (P.Id varName) -> [LF.symbol varName]
      P.FunType argType retType -> go argType ++ go retType
      P.DataType _ typArgs -> fromTypeDataArgs typArgs
      _ -> []

    fromTypeDataArgs args = flip concatMap args $
        \(P.DataTypeArg arg) -> go arg
