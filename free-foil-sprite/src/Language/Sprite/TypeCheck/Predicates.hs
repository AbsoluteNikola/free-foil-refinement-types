module Language.Sprite.TypeCheck.Predicates where
import Language.Sprite.Syntax
import Control.Monad.Foil qualified as F
import Control.Monad.Free.Foil qualified as F
import qualified Language.Sprite.Syntax.Inner.Abs as Inner

constIntT :: Integer -> Term F.VoidS
constIntT x = F.withFreshBinder F.emptyScope $
  \binder -> TypeRefined Inner.BaseTypeInt (PatternVar binder) (OpExpr (F.Var (F.nameOf binder)) Inner.EqOp (ConstInt x))

data BinOpType startScope where
  BinOpType :: (F.DExt startScope n, F.DExt n l) =>
    -- | xBinder
    F.NameBinder startScope n ->
    -- | xType
    Term startScope ->
    -- | yBinder
    F.NameBinder n l ->
    -- | yType
    Term n ->
    -- | resType
    Term l ->
    BinOpType startScope

binOpTypes :: F.Distinct o => F.Scope o -> Inner.Op -> BinOpType o
binOpTypes startScope op = F.withFreshBinder startScope $ \xBinder ->
  let
    scopeWithX = F.extendScope xBinder startScope
    xPat = PatternVar xBinder
    xType =  TypeRefined Inner.BaseTypeInt xPat ConstTrue
  in
    case (F.assertDistinct xBinder, F.assertExt xBinder) of
      (F.Distinct, F.Ext) ->
        F.withFreshBinder scopeWithX $ \yBinder ->
          case (F.assertDistinct yBinder, F.assertExt yBinder) of
          (F.Distinct, F.Ext) ->
              let
                scopeWithY = F.extendScope yBinder scopeWithX
                yPat = PatternVar yBinder
                yType =  TypeRefined Inner.BaseTypeInt yPat ConstTrue
              in
                F.withFreshBinder scopeWithY $ \opResBinder ->
                  case (F.assertDistinct opResBinder, F.assertExt opResBinder) of
                   (F.Distinct, F.Ext) ->
                      let
                        resType =
                          TypeRefined
                            Inner.BaseTypeInt
                            (PatternVar opResBinder)
                            (OpExpr
                              (F.Var . F.nameOf $ opResBinder) Inner.EqOp
                              (OpExpr (F.Var (F.sink . F.nameOf $ xBinder)) op (F.Var (F.sink . F.nameOf $ yBinder))))
                      in
                        BinOpType xBinder xType yBinder yType resType
