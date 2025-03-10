{-# LANGUAGE TupleSections #-}
module Language.Sprite.TypeCheck.Types where

import Language.Sprite.Syntax
import Control.Monad.Foil qualified as F
import Control.Monad.Free.Foil qualified as F
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import Language.Sprite.TypeCheck.Monad
import Control.Monad.Error.Class (MonadError(throwError))
import Data.Maybe (mapMaybe)
import Language.Sprite.TypeCheck.Constraints (baseTypeToSort)

constIntT :: Integer -> Term F.VoidS
constIntT x = F.withFreshBinder F.emptyScope $
  \binder -> TypeRefined Inner.BaseTypeInt (PatternVar binder) (OpExpr (F.Var (F.nameOf binder)) Inner.EqOp (ConstInt x))

anyIntT :: Term F.VoidS
anyIntT = F.withFreshBinder F.emptyScope $ \binder ->
    TypeRefined Inner.BaseTypeInt (PatternVar binder) (Boolean Inner.ConstTrue)

anyBoolT :: Term F.VoidS
anyBoolT = F.withFreshBinder F.emptyScope $ \binder ->
    TypeRefined Inner.BaseTypeBool (PatternVar binder) (Boolean Inner.ConstTrue)

{- |  see 4.3.2 Synthesis and figure 4.3

G(x) = b[v|p] -> b[v|p && v = x]
-}
singletonT ::
  (F.Distinct i) =>
  -- | var name
  F.Name i ->
  -- | var type
  Term i ->
  --  | type with singleton Type Strengthening
  Term i
singletonT varName typ = case typ of
  TypeRefined base (PatternVar typVar) predicate ->
    case (F.assertDistinct typVar, F.assertExt typVar) of
      (F.Distinct, F.Ext) -> TypeRefined base (PatternVar typVar)
        (OpExpr predicate Inner.AndOp
          (OpExpr (F.Var (F.sink varName)) Inner.EqOp (F.Var (F.nameOf typVar))))
  _ -> typ

{- See 5.4, Figure 5.4, page 34 -}
fresh ::F.Distinct i => F.Scope i -> Env i -> Term i -> CheckerM (Term i)
fresh scope env curType = case curType of
  {-
  fresh(G, b[v|p]) = b[v|p]
  -}
  t@TypeRefined{} -> pure t

  {-
  fresh(G, x:s -> t) = x:s' -> t'
    s' = fresh(G, s)
    t' = fresh(G; x:s, t)
  -}
  TypeFun (PatternVar argBinder) artTyp retTyp -> do
    argType' <- fresh scope env artTyp
    retType' <- case (F.assertDistinct argBinder, F.assertExt argBinder) of
      (F.Distinct, F.Ext) ->
        withExtendedEnv env argBinder artTyp $ \env' -> do
          let scope' = F.extendScope argBinder scope
          fresh scope' env' retTyp
    pure $ TypeFun (PatternVar argBinder) argType' retType'

  {-
  fresh(G, b[?]) = b[v| k(v,...x) ]
    k = fresh horn variable of sorts b : ...x sorts
    v = fresh binder
    ...x = arguments from env
  -}
  TypeRefinedUnknown base -> do
    let
      (names, sorts) = unzip $ flip mapMaybe (envToList env) $
        \(name, typ) -> (name, ) . baseTypeToSort <$> getBaseType typ
    newHornVarName <- mkFreshHornVar (baseTypeToSort base : sorts)

    F.withFreshBinder scope $ \freshBinder ->
      case (F.assertDistinct freshBinder, F.assertExt freshBinder) of
        (F.Distinct, F.Ext) -> pure $
          TypeRefined
            base
            (PatternVar freshBinder) -- v
            -- k(v,...x)
            (HVar newHornVarName $ F.Var (F.nameOf freshBinder) : (F.Var . F.sink <$> names ))

  otherTerm -> throwError $
    "fresh should be called only on type, not term:\n" <> pShowT otherTerm


getBaseType :: Term i -> Maybe Inner.BaseType
getBaseType = \case
  TypeRefined base _ _ -> Just base
  TypeRefinedUnknown base -> Just base
  _ -> Nothing
