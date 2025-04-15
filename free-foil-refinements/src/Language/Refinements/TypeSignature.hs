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
import Data.Functor ((<&>))

typeToSort :: P.Type -> LF.Sort
typeToSort typ = go 0 typ
  where
    go :: Int -> P.Type -> LF.Sort
    go forallIndex = \case
      P.BoolType -> LF.boolSort
      P.IntType -> LF.intSort
      P.VarType (P.Id varName) -> LF.FObj (LF.symbol varName)
      P.DataType (P.Id typName) args -> LF.fAppTC
        (LF.symbolFTycon . LF.dummyLoc  . LF.symbol $ typName)
        (args <&> \(P.DataTypeArg argTyp) -> go forallIndex argTyp)
      P.FunType argTyp retTyp ->
        LF.FFunc (go forallIndex argTyp) (go forallIndex retTyp)
      P.ForallType (P.Id varName) typUnderForall ->
        let
          typeUnderForAllSort = go (forallIndex + 1) typUnderForall
          subst = LF.mkSortSubst [(LF.symbol varName, LF.FVar forallIndex)]
          s = LF.sortSubst subst typeUnderForAllSort
        in LF.FAbs forallIndex s
