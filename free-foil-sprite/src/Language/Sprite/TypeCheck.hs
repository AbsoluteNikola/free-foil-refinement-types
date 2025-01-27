{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Sprite.TypeCheck where

import qualified Control.Monad.Foil as Foil
import qualified Control.Monad.Foil.Internal as Foil
import Control.Monad.Free.Foil (AST(..))

import Language.Sprite.Syntax

type TypeCheck = Maybe

type Env n = Foil.NameMap n (Term n)

deriving instance Functor (Foil.NameMap n)

check :: Foil.Distinct n => Env n -> Term n -> Term n -> TypeCheck ()
check env expr t2 =
  case expr of
    Let (PatternVar binder) e1 e2 -> do
      case (Foil.assertExt binder, Foil.assertDistinct binder) of
        (Foil.Ext, Foil.Distinct) -> do
          type1 <- synthesize env e1
          -- binder :: NameBinder n l
          -- type1 :: Type n
          -- env :: NameMap n (Type n)
          -- env' :: NameMap l (Type l)
          let env' = Foil.sink <$> Foil.addNameBinder binder type1 env
          check env' e2 (Foil.sink t2)
    TypeFun{} -> error "unexpected type in place of expression"
    _ -> error "not implemented"

synthesize :: Env n -> Term n -> TypeCheck (Term n)
synthesize env = \case
  Var name -> return (Foil.lookupName name env)
  _ -> error "not implemented"
