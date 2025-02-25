module Language.Sprite.TypeCheck.Predicates where
import Language.Sprite.Syntax
import Control.Monad.Foil qualified as F
import Control.Monad.Free.Foil qualified as F
import qualified Language.Sprite.Syntax.Inner.Abs as Inner

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
    (argumentBaseType, resultBaseType) = getBaseTypesForOp op
    scopeWithX = F.extendScope xBinder startScope
    xPat = PatternVar xBinder
    xType =  TypeRefined argumentBaseType xPat (Boolean Inner.ConstTrue)
  in
    case (F.assertDistinct xBinder, F.assertExt xBinder) of
      (F.Distinct, F.Ext) ->
        F.withFreshBinder scopeWithX $ \yBinder ->
          case (F.assertDistinct yBinder, F.assertExt yBinder) of
          (F.Distinct, F.Ext) ->
              let
                scopeWithY = F.extendScope yBinder scopeWithX
                yPat = PatternVar yBinder
                yType =  TypeRefined argumentBaseType yPat (Boolean Inner.ConstTrue)
              in
                F.withFreshBinder scopeWithY $ \opResBinder ->
                  case (F.assertDistinct opResBinder, F.assertExt opResBinder) of
                   (F.Distinct, F.Ext) ->
                      let
                        resType =
                          TypeRefined
                            resultBaseType
                            (PatternVar opResBinder)
                            (OpExpr
                              (F.Var . F.nameOf $ opResBinder) Inner.EqOp
                              (OpExpr (F.Var (F.sink . F.nameOf $ xBinder)) op (F.Var (F.sink . F.nameOf $ yBinder))))
                      in
                        BinOpType xBinder xType yBinder yType resType

getBaseTypesForOp :: Inner.Op -> (Inner.BaseType {- Arguments base type-}, Inner.BaseType {- result base type-})
getBaseTypesForOp = \case
  Inner.PlusOp -> (Inner.BaseTypeInt, Inner.BaseTypeInt)
  Inner.MinusOp -> (Inner.BaseTypeInt, Inner.BaseTypeInt)
  Inner.MultiplyOp -> (Inner.BaseTypeInt, Inner.BaseTypeInt)
  Inner.EqOp -> (Inner.BaseTypeInt, Inner.BaseTypeBool)
  Inner.LessOp -> (Inner.BaseTypeInt, Inner.BaseTypeBool)
  Inner.LessOrEqOp -> (Inner.BaseTypeInt, Inner.BaseTypeBool)
  Inner.GreaterOp -> (Inner.BaseTypeInt, Inner.BaseTypeBool)
  Inner.GreaterOrEqOp -> (Inner.BaseTypeInt, Inner.BaseTypeBool)
  Inner.AndOp -> (Inner.BaseTypeBool, Inner.BaseTypeBool)
  Inner.OrOp -> (Inner.BaseTypeBool, Inner.BaseTypeBool)
