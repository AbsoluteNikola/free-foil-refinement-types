{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Refinements.Measure where

import qualified Language.Refinements.Predicates as P
import qualified Language.Fixpoint.Types as LF
import Data.Functor ((<&>))
import qualified Language.Refinements.Constraint as C
import qualified Control.Monad.Free.Foil as Foil
import Language.Refinements.TypeSignature (typeToSort)

data Measure = Measure
  { measureName :: String
  , sort :: LF.Sort
  }

mkMeasure :: C.IsType sig binder => String -> Foil.AST binder sig n -> Measure
mkMeasure name typ = Measure name (typeToSort $ C.toTypeSignature typ)

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
