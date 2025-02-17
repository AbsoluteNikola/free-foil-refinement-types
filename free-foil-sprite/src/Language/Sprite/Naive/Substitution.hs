module Language.Sprite.Naive.Substitution where
import Language.Sprite.Syntax.Front.Abs (VarIdent, Pred(..), RType(..), FuncArg (..), ScopedRType (..))

-- 3.3.1, page 11
substType :: VarIdent -> Pred -> RType -> RType
substType varId whatP typ = case typ of
  TypeRefined b typVarId typPred
    | varId == typVarId -> typ
    | otherwise -> TypeRefined b typVarId (substPred varId whatP typPred)
  TypeFun (NamedFuncArg argId argTyp) (ScopedRType returnType)
    | argId == varId ->
      TypeFun
        (NamedFuncArg argId $ substType varId whatP argTyp)
        (ScopedRType returnType)
    | otherwise -> TypeFun
        (NamedFuncArg argId $ substType varId whatP argTyp)
        (ScopedRType $ substType varId whatP returnType)

substPred :: VarIdent -> Pred -> Pred -> Pred
substPred varId whatP toP = case toP of
  PVar innerVarId
    | varId == innerVarId -> whatP
    | otherwise -> toP
  PBool{} -> toP
  PInt _ -> toP
  PEq lp rp -> PEq (substPred varId whatP lp) (substPred varId whatP rp)
  PGreaterThan lp rp -> PGreaterThan (substPred varId whatP lp) (substPred varId whatP rp)
  PGreaterOrEqThan lp rp -> PGreaterOrEqThan (substPred varId whatP lp) (substPred varId whatP rp)
  PLessThan lp rp -> PLessThan (substPred varId whatP lp) (substPred varId whatP rp)
  PLessOrEqThan lp rp -> PLessOrEqThan (substPred varId whatP lp) (substPred varId whatP rp)
  PPlus lp rp -> PPlus (substPred varId whatP lp) (substPred varId whatP rp)
  PMinus lp rp -> PMinus (substPred varId whatP lp) (substPred varId whatP rp)
  PMultiply lp rp -> PMultiply (substPred varId whatP lp) (substPred varId whatP rp)
