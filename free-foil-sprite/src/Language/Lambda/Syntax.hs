{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module Language.Lambda.Syntax where

-- import Control.Monad.Free.Foil
import Control.Monad.Free.Foil.TH.MkFreeFoil
import Language.Lambda.FreeFoilConfig (lambdaConfig)

mkFreeFoil lambdaConfig
