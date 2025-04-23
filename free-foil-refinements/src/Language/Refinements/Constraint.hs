{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}

module Language.Refinements.Constraint where

import Data.Text (Text)
import qualified Language.Refinements.Predicates.Abs as P
import qualified Language.Fixpoint.Types as LF
import qualified Language.Fixpoint.Horn.Types as LF
import qualified Language.Refinements.Predicates as P
import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as FoilInteral
import qualified Control.Monad.Free.Foil as Foil
import Data.Bifunctor (bimap, Bifunctor)
import Data.Bitraversable (Bitraversable)
import Language.Refinements.TypeSignature (typeToSort)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as Text
import Debug.Trace (trace, traceShowId)

-- Env

data Env sig binder n where
    EmptyEnv :: Env sig binder Foil.VoidS
    NonEmptyEnv ::
      (Foil.CoSinkable binder, Foil.DExt i o) =>
      Env sig binder i ->
      Foil.NameBinder i o ->
      Foil.AST binder sig i ->
      Env sig binder o

withExtendedEnv :: (Foil.DExt i o, Foil.CoSinkable binder) => Env sig binder i -> Foil.NameBinder i o -> Foil.AST binder sig i -> (Env sig binder o -> m a) -> m a
withExtendedEnv env binder typ action = do
  let env' = NonEmptyEnv env binder typ
  action env'

lookupEnvWithStrengthening ::
  (Foil.Distinct o, Bifunctor sig, IsType sig binder, Foil.CoSinkable binder) =>
  Foil.Scope o ->
  Env sig binder o ->
  Foil.Name o ->
  Foil.AST binder sig o
lookupEnvWithStrengthening scope env name = singletonT name $ Foil.refreshAST scope resTyp
  where
    resTyp = lookupEnv env name

lookupEnv :: (Bifunctor sig, IsType sig binder) => Env sig binder o -> Foil.Name o -> Foil.AST binder sig o
lookupEnv env varId = case env of
  EmptyEnv -> error $ "impossible case, no var in env: " <> show varId
  NonEmptyEnv env' binder term ->
    if Foil.nameOf binder == varId
      then singletonT varId $ Foil.sink term
      else
        case Foil.unsinkName binder varId of
          Nothing -> error $ "impossible case. If we didn't found var in bigger scoped map. It should be in smaller scope " <> show varId
          Just varId' -> Foil.sink $ lookupEnv env' varId'

changeVarTypeInEnv :: (Foil.Distinct o, Foil.CoSinkable binder) => Env sig binder o -> Foil.Name o -> Foil.AST binder sig o -> Env sig binder  o
changeVarTypeInEnv env varId = NonEmptyEnv env (FoilInteral.UnsafeNameBinder varId)

envToList :: Bifunctor sig => Env sig binder o  -> [(Foil.Name o, Foil.AST binder sig o)]
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

cTrue :: Constraint
cTrue = CAnd []

-- equality to build implication from type
сImplicationFromType ::
  (IsType sig binder, Foil.Distinct n, Bifunctor sig, Foil.CoSinkable binder) =>
  Foil.Scope n ->
  Foil.NameBinder n l ->
  Foil.AST binder sig n ->
  Constraint ->
  Text ->
  Constraint
сImplicationFromType scope varBinder typ constraint msg = fromMaybe constraint mImplConstraint
  where
    mImplConstraint = fst $ withPred typ $ \wp@(WithPred nameBinder p) ->
      case (Foil.assertDistinct varBinder, Foil.assertExt varBinder) of
        (Foil.Distinct, Foil.Ext) ->
          let
            sort = typeToSort $ toTypeSignature typ
            scope' = Foil.extendScope varBinder scope
            subst = Foil.addRename (Foil.sink Foil.identitySubst) nameBinder (Foil.nameOf varBinder)
            p' = Foil.substitute scope' subst p
            rawVarId = Text.pack $ "x" <> show (Foil.nameId $ Foil.nameOf varBinder)
          in (CImplication rawVarId sort (toPredicate p') constraint msg, wp)

cPred :: IsPred sig binder => Foil.AST binder sig n -> Text -> Constraint
cPred p = CPred (toPredicate p)

cAnd ::[Constraint] -> Constraint
cAnd = CAnd

-- Working with types and predicates
data WithPred sig binder (i :: Foil.S) where
  WithPred :: (IsPred sig binder, Foil.DExt i o, Show (Foil.AST binder sig o)) => Foil.NameBinder i o -> Foil.AST binder sig o -> WithPred sig binder i

class IsType sig binder where
  withPred ::
    Foil.Distinct n =>
    Foil.AST binder sig n ->
    (WithPred sig binder n -> (a, WithPred sig binder n)) ->
    (Maybe a, Foil.AST binder sig n)
  toTypeSignature :: Foil.AST binder sig n -> P.Type

class IsPred sig binder where
  isUnknown :: Foil.AST binder sig n  -> Bool
  mkAnd :: Foil.AST binder sig n -> Foil.AST binder sig n -> Foil.AST binder sig n
  mkEq :: Foil.AST binder sig n -> Foil.AST binder sig n -> Foil.AST binder sig n
  mkHornVar :: String -> [Foil.Name n] -> Foil.AST binder sig n
  toPredicate :: Foil.AST binder sig n -> P.Pred

singletonT :: (Foil.Distinct n, IsType sig binder) => Foil.Name n -> Foil.AST binder sig n -> Foil.AST binder sig n
singletonT varName typ = snd $ withPred typ $ \(WithPred pNameBinder p) ->
  case (Foil.assertDistinct pNameBinder, Foil.assertExt pNameBinder) of
    (Foil.Distinct, Foil.Ext) ->
      let
        msg = "VarName: " <> show varName
          <> " Binder: " <> show (Foil.nameOf pNameBinder)
          <> " Old pattern" <> show p
          <> " New pattern" <> show newPred <> "\n"
        newPred = mkAnd
            p
            ( mkEq
              (Foil.Var $ Foil.sink $ varName)
              (Foil.Var $ Foil.nameOf pNameBinder)
            )
      in ((), trace msg $  WithPred pNameBinder newPred)

freshTypeWithPredicate ::
  (Bitraversable sig, Foil.Distinct n, IsType sig binder) =>
  RefinementCheckState ->
  Env sig binder n ->
  Foil.AST binder sig n ->
  (Foil.AST binder sig n, RefinementCheckState)
freshTypeWithPredicate refinementState env typToFresh =
  let
    envSorts = flip mapMaybe (envToList env) $ \(name, envTyp) ->
      fst $ withPred envTyp $ \withPredInst ->
        let sort = typeToSort (toTypeSignature envTyp)
        in ((name, sort), withPredInst)
    -- нужно проверить что у типа есть предикат и он Unknown
    mTypeSort =  fst $ withPred typToFresh $ \wp@(WithPred _ p) ->
      let
        sort = if isUnknown p
          then Just $ typeToSort (toTypeSignature typToFresh)
          else Nothing
      in (sort, wp)
  in case mTypeSort of
    Just (Just typSort) ->
      let
        (hornVarName, refinementState')
          = mkFreshHornVar refinementState (typSort : map snd envSorts)
        freshedType = snd $ withPred typToFresh $ \(WithPred pNameBinder _) ->
          let newPred = mkHornVar hornVarName (Foil.nameOf pNameBinder : map (Foil.sink . fst) envSorts)
          in ((), WithPred pNameBinder newPred)
      in (freshedType, refinementState')
    _ -> (typToFresh, refinementState)

newtype HornVar = HornVar { unHornVar :: LF.Var Text }

data RefinementCheckState = RefinementCheckState
  { nextHornVarIndex :: Int
  , hornVars :: [HornVar]
  }

emptyRefinementCheckState :: RefinementCheckState
emptyRefinementCheckState = RefinementCheckState 0 []

mkFreshHornVar :: RefinementCheckState -> [LF.Sort] -> (String, RefinementCheckState)
mkFreshHornVar refinementState sorts =
  let
    newIndex = refinementState.nextHornVarIndex
    varName = "k" <> show newIndex
    hv = LF.HVar (LF.symbol varName) sorts "fake"
    refinementState' = refinementState
      { nextHornVarIndex = newIndex + 1
      , hornVars = HornVar hv : refinementState.hornVars
      }
  in (varName, refinementState')
