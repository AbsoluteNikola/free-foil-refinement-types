{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}

module Language.Refinements.Constraint where

import Data.Text (Text)
import qualified Language.Refinements.Predicates.Abs as P
import qualified Language.Fixpoint.Types as LF
import Control.Monad.State (MonadState(..), gets, modify)
import qualified Language.Fixpoint.Horn.Types as LF
import qualified Language.Refinements.Predicates as P
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Free.Foil as Foil
import qualified Data.Kind
import Data.Bifunctor (bimap)

-- Env

data Env sig n where
    EmptyEnv :: Foil.Sinkable sig => Env sig Foil.VoidS
    NonEmptyEnv ::
      (Foil.Sinkable sig, Foil.DExt i o) =>
      Env sig i ->
      Foil.NameBinder i o ->
      sig i ->
      Env sig o

withExtendedEnv :: (Foil.DExt i o, Foil.Sinkable sig) => Env sig i -> Foil.NameBinder i o -> sig i -> (Env sig o -> m a) -> m a
withExtendedEnv env binder typ action = do
  let env' = NonEmptyEnv env binder typ
  action env'

lookupEnv :: Env sig o -> Foil.Name o -> sig o
lookupEnv env varId = case env of
  EmptyEnv -> error $ "impossible case, no var in env: " <> show varId
  NonEmptyEnv env' binder term ->
    if Foil.nameOf binder == varId
      then Foil.sink term
      else
        case Foil.unsinkName binder varId of
          Nothing -> error $ "impossible case. If we didn't found var in bigger scoped map. It should be in smaller scope " <> show varId
          Just varId' -> Foil.sink $ lookupEnv env' varId'

envToList :: Env sig o  -> [(Foil.Name o, sig o)]
envToList env = case env of
  EmptyEnv -> []
  NonEmptyEnv env' binder term ->
    (Foil.nameOf binder, Foil.sink term)
      : map (bimap Foil.sink  Foil.sink) (envToList env')

-- Constraints
data Constraint
  = CPred P.Pred Text
  | CAnd [Constraint]
  | CImplication Text LF.Sort P.Pred Constraint Text
  deriving (Show)

constraintsToLF :: Constraint -> Either P.ConvertError (LF.Cstr Text)
constraintsToLF = \case
  CPred p msg -> do
    pred' <- P.convertPredicate p
    pure $ LF.Head pred' msg
  CAnd cs -> do LF.CAnd <$> traverse constraintsToLF cs
  CImplication varId sort p c msg -> do
    p' <- P.convertPredicate p
    c' <- constraintsToLF c
    pure $ LF.All
      (LF.Bind
        (LF.symbol varId)
        sort
        p'
        msg)
      c'

class Monad m => MonadConstraints m where
  addConstraint :: Constraint -> m ()
  getConstraint :: m Constraint

instance (Monad m, MonadState Constraint m) => MonadConstraints m where
  addConstraint newC = do
    oldC <- get
    put $ case oldC of
      CAnd cs -> CAnd (cs ++ [newC])
      _       -> CAnd [oldC, newC]
  getConstraint = get

cTrue :: Constraint
cTrue = CAnd []

-- equality to build implication from type
сImplication :: MonadConstraints m => m ()
сImplication = undefined

cPred :: MonadConstraints m => P.Pred -> Text -> m ()
cPred p msg = addConstraint $ CPred p msg

cAnd :: MonadConstraints m => [m a] -> m [a]
cAnd = sequenceA

-- Working with types and predicates
data WithPred sig binder (i :: Foil.S) where
  WithPred :: (IsPred sig binder, Foil.DExt i o) => Foil.NameBinder i o -> Foil.AST binder sig o -> WithPred sig binder i

class IsType sig binder where
  withPred ::
    Foil.Distinct n =>
    Foil.AST binder sig n ->
    (WithPred sig binder n -> WithPred sig binder n) ->
    Foil.AST binder sig n
  toTypeSignature :: Foil.AST binder sig n -> P.Type

class IsPred sig binder where
  isUnknown :: Foil.AST binder sig n  -> Bool
  mkAnd :: Foil.AST binder sig n -> Foil.AST binder sig n -> Foil.AST binder sig n
  mkEq :: Foil.AST binder sig n -> Foil.AST binder sig n -> Foil.AST binder sig n
  mkHornVar :: [Foil.Name n] -> Foil.AST binder sig n
  toPredicate :: Foil.AST binder sig n -> P.Pred

singletonT :: (Foil.Distinct n, IsType sig binder) => Foil.Name n -> Foil.AST binder sig n -> Foil.AST binder sig n
singletonT varName typ = withPred typ $ \(WithPred pNameBinder p) ->
  case (Foil.assertDistinct pNameBinder, Foil.assertExt pNameBinder) of
    (Foil.Distinct, Foil.Ext) ->
        WithPred pNameBinder
        ( mkAnd
          p
          ( mkEq
            (Foil.Var $ Foil.sink varName)
            (Foil.Var $ Foil.nameOf pNameBinder)
          )
        )

fresh :: (Foil.Distinct n, IsType sig binder) => Foil.Name n -> Foil.AST binder sig n -> Foil.AST binder sig n
fresh = undefined

newtype HornVar = HornVar (LF.Var Text)
newtype HornVarName = HornVarName String

data RefinementCheckState = RefinementCheckState
  { nextHornVarIndex :: Int
  , hornVars :: [HornVar]
  }

mkFreshHornVar :: MonadState RefinementCheckState m => [LF.Sort] -> m HornVarName
mkFreshHornVar sorts = do
  newIndex <- gets (.nextHornVarIndex)
  let varName = "k" <> show newIndex
  let hv = LF.HVar (LF.symbol varName) sorts "fake"
  modify $ \s ->
     s
     { nextHornVarIndex = newIndex + 1
     , hornVars = HornVar hv : s.hornVars
     }
  pure $ HornVarName varName
