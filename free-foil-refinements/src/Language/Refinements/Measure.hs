{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Refinements.Measure where

import qualified Language.Refinements.Predicates.Abs as P
import qualified Language.Refinements.Predicates as P
import qualified Language.Fixpoint.Types as LF
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Foldable (foldl')
import qualified Language.Refinements.Constraint as C
import qualified Control.Monad.Free.Foil as Foil

data Measure = Measure
  { measureName :: String
  , sort :: LF.Sort
  }

mkMeasure :: C.IsType sig binder => String -> Foil.AST binder sig n -> Measure
mkMeasure name typ = Measure name (typeToSort $ C.toTypeSignature typ)

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

data Qualifier = Qualifier
  { name :: String
  , params :: [(String, LF.Sort)]
  , body :: LF.Expr
  }

mkQualifier :: (C.IsType t binder, C.IsPred p binder) => String -> [(String, Foil.AST binder t n)] -> Foil.AST binder p k -> Qualifier
mkQualifier qualName args predicate = Qualifier qualName qualArgs $
  case P.convertExpr (C.toPredicate predicate) of
    Left err -> error (show err)
    Right p -> p
  where
    qualArgs = args <&> \(argName, t) ->
      ( argName
      , typeToSort $ C.toTypeSignature t
      )

convertQualifier :: Qualifier -> LF.Qualifier
convertQualifier Qualifier{..} = LF.Q
    { qName = LF.symbol name
    , qParams = qualArgs
    , qBody = body
    , qPos = LF.dummyPos ""
    }
  where
    qualArgs = params <&> \(argName, sort) -> LF.QP
        { qpSym = LF.symbol argName
        , qpPat = LF.PatNone
        , qpSort = sort
        }
