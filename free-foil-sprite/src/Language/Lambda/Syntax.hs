{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module Language.Lambda.Syntax where

-- import Control.Monad.Free.Foil
import Data.Bifunctor.TH
import qualified Control.Monad.Foil as Foil
import Control.Monad.Free.Foil.TH.MkFreeFoil
import Language.Lambda.FreeFoilConfig (lambdaConfig)

mkFreeFoil lambdaConfig
deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig

instance Foil.CoSinkable Pattern where
  coSinkabilityProof rename (PatternVar binder) cont =
    Foil.coSinkabilityProof rename binder $ \rename' binder' ->
      cont rename' (PatternVar binder')

  withPattern withBinder _id _comp scope (PatternVar binder) cont =
    withBinder scope binder $ \f' binder' ->
      cont f' (PatternVar binder')

type Expr = Term
type Type = Term
type Predicate = Term
