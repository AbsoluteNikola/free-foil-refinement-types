{-# LANGUAGE TemplateHaskell #-}
module Language.Lambda.FreeFoilConfig where

import qualified Language.Lambda.Syntax.Abs    as Raw
import           Control.Monad.Free.Foil.TH.MkFreeFoil

intToVarIdent :: Int -> Raw.VarIdent
intToVarIdent i = Raw.VarIdent ("x" <> show i)

rawVar :: Raw.VarIdent -> Raw.Term
rawVar = Raw.Var

rawScopedTerm :: Raw.Term -> Raw.ScopedTerm
rawScopedTerm = Raw.ScopedTerm

rawScopeToTerm :: Raw.ScopedTerm -> Raw.Term
rawScopeToTerm (Raw.ScopedTerm expr) = expr

lambdaConfig :: FreeFoilConfig
lambdaConfig = FreeFoilConfig
  { rawQuantifiedNames = [ ]
  , freeFoilTermConfigs =
      [ FreeFoilTermConfig
          { rawIdentName = ''Raw.VarIdent
          , rawTermName = ''Raw.Term
          , rawBindingName = ''Raw.Pattern
          , rawScopeName = ''Raw.ScopedTerm
          , rawVarConName = 'Raw.Var
          , rawSubTermNames = [ ]
          , rawSubScopeNames = [ ]
          , intToRawIdentName = 'intToVarIdent
          , rawVarIdentToTermName = 'rawVar
          , rawTermToScopeName = 'rawScopedTerm
          , rawScopeToTermName = 'rawScopeToTerm
          }
      ]
  , freeFoilNameModifier = id
  , freeFoilScopeNameModifier = ("Scoped" ++ )
  , freeFoilConNameModifier = id
  , freeFoilConvertFromName = ("from" ++ )
  , freeFoilConvertToName = ("to" ++ )
  , signatureNameModifier = (++ "Sig")
  }
