module Language.Sprite.Naive.Constraints where

import Language.Sprite.Syntax.Abs (Pred, VarIdent, BaseType, RType(..))

data Constraint
  = CPred Pred
  | CAnd [Constraint]
  | CImplication VarIdent BaseType Pred Constraint

cTrue :: Constraint
cTrue = CAnd []

--  id:b[pred] => constraint. See page 17
buildImplicationFromType :: RType -> Constraint -> Constraint
buildImplicationFromType typ constraint = case typ of
  TypeRefined base varId p ->
    CImplication varId base p constraint
  TypeFun{} -> constraint
