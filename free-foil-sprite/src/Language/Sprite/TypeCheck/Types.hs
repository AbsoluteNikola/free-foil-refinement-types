module Language.Sprite.TypeCheck.Types where

import Language.Sprite.Syntax
import Control.Monad.Foil qualified as F
import Control.Monad.Free.Foil qualified as F
import qualified Language.Sprite.Syntax.Inner.Abs as Inner

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
