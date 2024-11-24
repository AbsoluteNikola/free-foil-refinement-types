{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module Language.Sprite.Syntax where

-- import Control.Monad.Free.Foil
import Data.String (IsString(..))
import qualified Data.Map as Map
import qualified Language.Sprite.Syntax.Print as Raw
import qualified Language.Sprite.Syntax.Par as Raw
import Data.Bifunctor.TH
import qualified Control.Monad.Foil as Foil
import Control.Monad.Free.Foil.TH.MkFreeFoil
import Language.Sprite.FreeFoilConfig (spriteConfig)

-- mkFreeFoil spriteConfig

-- deriveBifunctor ''TermSig
-- deriveBifoldable ''TermSig
-- deriveBitraversable ''TermSig

-- instance Foil.CoSinkable Pattern where
--   coSinkabilityProof rename (PatternVar binder) cont =
--     Foil.coSinkabilityProof rename binder $ \rename' binder' ->
--       cont rename' (PatternVar binder')

--   withPattern withBinder _id _comp scope (PatternVar binder) cont =
--     withBinder scope binder $ \f' binder' ->
--       cont f' (PatternVar binder')

-- mkFreeFoilConversions lambdaConfig

-- type Expr = Term
-- type Type = Term
-- type Predicate = Term

-- -- >>> "(x) => { let x = y; y }" :: Expr Foil.VoidS
-- -- (x0) =>
-- -- {
-- --   let x0 = x1;
-- --   x1
-- -- }
-- instance Show (Term n) where
--   show = Raw.printTree . fromTerm

-- instance IsString (Term Foil.VoidS) where
--   fromString = toTerm Foil.emptyScope Map.empty . unsafeParseTerm
--     where
--       unsafeParseTerm input =
--         case Raw.pTerm (Raw.myLexer input) of
--           Left err -> error err
--           Right term -> term
