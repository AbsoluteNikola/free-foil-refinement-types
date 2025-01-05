{-# OPTIONS_GHC -ddump-splices #-}
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

import Data.String (IsString(..))
import qualified Data.Map as Map
import qualified Language.Sprite.Syntax.Inner.Print as Raw
import qualified Language.Sprite.Syntax.Inner.Par as Raw
import Data.Bifunctor.TH
    ( deriveBifoldable, deriveBifunctor, deriveBitraversable )
import qualified Control.Monad.Foil as Foil
-- import Control.Monad.Free.Foil.TH.MkFreeFoil
import Language.Sprite.FreeFoilConfig
import qualified Language.Sprite.Syntax.Inner.Abs
import qualified GHC.Generics
import qualified Control.Monad.Free.Foil

-- There is issue with generating patterns and instances for AST nodes with pattern and multiple Terms/Scoped terms.
-- Code below just copy paste of generated code via TH and some fixes. I hope I will find some time to fix it in free-foil
-- the same issue with mkFreeFoilConversions
-- mkFreeFoil spriteConfig
data Pattern o i
  where
    PatternVar :: (Foil.NameBinder o i0_a91x) -> Pattern o i0_a91x
data TermSig scope term
  where
    ConstIntSig :: Integer -> TermSig scope term
    LetSig :: term -> scope -> TermSig scope term
    FunSig :: scope -> TermSig scope term
    AppSig :: term -> term -> TermSig scope term
    AnnSig :: term -> term -> TermSig scope term
    TypeRefinedSig :: Language.Sprite.Syntax.Inner.Abs.BaseType ->
                      scope ->
                      TermSig scope term
    TypeFunSig :: term -> scope -> TermSig scope term
    ConstTrueSig :: TermSig scope term
    ConstFalseSig :: TermSig scope term
    PEqSig :: term -> term -> TermSig scope term
    PLessOrEqThanSig :: term -> term -> TermSig scope term
    PLessThanSig :: term -> term -> TermSig scope term
    PlusSig :: term -> term -> TermSig scope term
    MinusSig :: term -> term -> TermSig scope term
    MultiplySig :: term -> term -> TermSig scope term
  deriving (GHC.Generics.Generic, Functor, Foldable, Traversable)

type Term = Control.Monad.Free.Foil.AST Pattern TermSig
type ScopedTerm = Control.Monad.Free.Foil.ScopedAST Pattern TermSig

pattern ConstInt :: Integer -> Term o
pattern ConstInt x_a91y = Control.Monad.Free.Foil.Node (ConstIntSig x_a91y)
pattern Let ::  Pattern o i -> Term o -> Term i -> Term o
pattern Let binder_a91A x_a91z body_a91B = Control.Monad.Free.Foil.Node (LetSig x_a91z (Control.Monad.Free.Foil.ScopedAST binder_a91A body_a91B))

-- FIXED HERE
pattern Fun :: Pattern o i -> Term i -> Term o
pattern Fun binder_a91C body_a91D = Control.Monad.Free.Foil.Node (FunSig (Control.Monad.Free.Foil.ScopedAST binder_a91C body_a91D))
pattern App :: Term o -> Term o -> Term o
pattern App x_a91E x_a91F = Control.Monad.Free.Foil.Node (AppSig x_a91E
                                                                  x_a91F)
pattern Ann :: Term o -> Term o -> Term o
pattern Ann x_a91G x_a91H = Control.Monad.Free.Foil.Node (AnnSig x_a91G x_a91H)
pattern TypeRefined ::
          Language.Sprite.Syntax.Inner.Abs.BaseType
          -> Pattern o i -> Term i -> Term o
pattern TypeRefined x_a91I binder_a91J body_a91K = Control.Monad.Free.Foil.Node (TypeRefinedSig x_a91I (Control.Monad.Free.Foil.ScopedAST binder_a91J body_a91K))
 -- FIXED HERE
pattern TypeFun :: Pattern o i -> Term o  -> Term i -> Term o
pattern TypeFun binder_a91M x_a91L body_a91N = Control.Monad.Free.Foil.Node (TypeFunSig x_a91L (Control.Monad.Free.Foil.ScopedAST binder_a91M body_a91N))
pattern ConstTrue :: Term o
pattern ConstTrue = Control.Monad.Free.Foil.Node ConstTrueSig
pattern ConstFalse :: Term o
pattern ConstFalse = Control.Monad.Free.Foil.Node ConstFalseSig
pattern PEq :: Term o -> Term o -> Term o
pattern PEq x_a91O x_a91P = Control.Monad.Free.Foil.Node (PEqSig x_a91O x_a91P)
pattern PLessOrEqThan :: Term o -> Term o -> Term o
pattern PLessOrEqThan x_a91Q x_a91R = Control.Monad.Free.Foil.Node (PLessOrEqThanSig x_a91Q x_a91R)
pattern PLessThan :: Term o -> Term o -> Term o
pattern PLessThan x_a91S x_a91T = Control.Monad.Free.Foil.Node (PLessThanSig x_a91S x_a91T)
pattern Plus :: Term o -> Term o -> Term o
pattern Plus x_a91U x_a91V = Control.Monad.Free.Foil.Node (PlusSig x_a91U x_a91V)
pattern Minus :: Term o -> Term o -> Term o
pattern Minus x_a91W x_a91X = Control.Monad.Free.Foil.Node (MinusSig x_a91W x_a91X)
pattern Multiply :: Term o -> Term o -> Term o
pattern Multiply x_a91Y x_a91Z = Control.Monad.Free.Foil.Node (MultiplySig x_a91Y                                                                x_a91Z)
{-# COMPLETE Control.Monad.Free.Foil.Var, ConstInt, Let, Fun, App, Ann, TypeRefined, TypeFun, ConstTrue, ConstFalse, PEq, PLessOrEqThan, PLessThan, Plus, Minus, Multiply #-}

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


-- mkFreeFoilConversions spriteConfig
fromTermSig ::
  TermSig (Language.Sprite.Syntax.Inner.Abs.Pattern,
            Language.Sprite.Syntax.Inner.Abs.ScopedTerm) Language.Sprite.Syntax.Inner.Abs.Term
  -> Language.Sprite.Syntax.Inner.Abs.Term
fromTermSig (ConstIntSig x_achX)
  = Language.Sprite.Syntax.Inner.Abs.ConstInt x_achX
fromTermSig (LetSig x_achY (binder_achZ, body_aci0))
  = Language.Sprite.Syntax.Inner.Abs.Let binder_achZ x_achY body_aci0 -- FIXED HERE
fromTermSig (FunSig (binder_aci1, body_aci2))
  = Language.Sprite.Syntax.Inner.Abs.Fun binder_aci1 body_aci2
fromTermSig (AppSig x_aci3 x_aci4)
  = Language.Sprite.Syntax.Inner.Abs.App x_aci3 x_aci4
fromTermSig (AnnSig x_aci5 x_aci6)
  = Language.Sprite.Syntax.Inner.Abs.Ann x_aci5 x_aci6
fromTermSig (TypeRefinedSig x_aci7 (binder_aci8, body_aci9))
  = Language.Sprite.Syntax.Inner.Abs.TypeRefined
      x_aci7 binder_aci8 body_aci9
fromTermSig (TypeFunSig x_acia (binder_acib, body_acic))
  = Language.Sprite.Syntax.Inner.Abs.TypeFun
      binder_acib x_acia body_acic
fromTermSig ConstTrueSig
  = Language.Sprite.Syntax.Inner.Abs.ConstTrue
fromTermSig ConstFalseSig
  = Language.Sprite.Syntax.Inner.Abs.ConstFalse
fromTermSig (PEqSig x_acid x_acie)
  = Language.Sprite.Syntax.Inner.Abs.PEq x_acid x_acie
fromTermSig (PLessOrEqThanSig x_acif x_acig)
  = Language.Sprite.Syntax.Inner.Abs.PLessOrEqThan x_acif x_acig
fromTermSig (PLessThanSig x_acih x_acii)
  = Language.Sprite.Syntax.Inner.Abs.PLessThan x_acih x_acii
fromTermSig (PlusSig x_acij x_acik)
  = Language.Sprite.Syntax.Inner.Abs.Plus x_acij x_acik
fromTermSig (MinusSig x_acil x_acim)
  = Language.Sprite.Syntax.Inner.Abs.Minus x_acil x_acim
fromTermSig (MultiplySig x_acin x_acio)
  = Language.Sprite.Syntax.Inner.Abs.Multiply x_acin x_acio
fromPattern ::
  Pattern o i -> Language.Sprite.Syntax.Inner.Abs.Pattern
fromPattern (PatternVar x_acip)
  = Language.Sprite.Syntax.Inner.Abs.PatternVar
      (Language.Sprite.FreeFoilConfig.intToVarIdent
          (Foil.nameId (Foil.nameOf x_acip)))
fromTerm :: Term o -> Language.Sprite.Syntax.Inner.Abs.Term
fromTerm
  = Control.Monad.Free.Foil.convertFromAST
      fromTermSig Language.Sprite.FreeFoilConfig.rawVar fromPattern
      Language.Sprite.FreeFoilConfig.rawScopedTerm
      Language.Sprite.FreeFoilConfig.intToVarIdent
toTermSig ::
  Language.Sprite.Syntax.Inner.Abs.Term
  -> Either Language.Sprite.Syntax.Inner.Abs.VarIdent (TermSig (Language.Sprite.Syntax.Inner.Abs.Pattern,
                                                                Language.Sprite.Syntax.Inner.Abs.ScopedTerm) Language.Sprite.Syntax.Inner.Abs.Term)
toTermSig (Language.Sprite.Syntax.Inner.Abs.ConstInt _x_acir)
  = Right (ConstIntSig _x_acir)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Var _theRawIdent_acis)
  = Left _theRawIdent_acis
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.Let binder_aciv _x_aciu  -- FIXED HERE
                                        body_aciw)
  = Right (LetSig _x_aciu (binder_aciv, body_aciw))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.Fun binder_aciy body_aciz)
  = Right (FunSig (binder_aciy, body_aciz))
toTermSig (Language.Sprite.Syntax.Inner.Abs.App _x_aciB _x_aciC)
  = Right (AppSig _x_aciB _x_aciC)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Ann _x_aciE _x_aciF)
  = Right (AnnSig _x_aciE _x_aciF)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeRefined _x_aciH binder_aciI
                                                body_aciJ)
  = Right (TypeRefinedSig _x_aciH (binder_aciI, body_aciJ))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeFun binder_aciM _x_aciL  -- FIXED HERE
                                            body_aciN)
  = Right (TypeFunSig _x_aciL (binder_aciM, body_aciN))
toTermSig Language.Sprite.Syntax.Inner.Abs.ConstTrue
  = Right ConstTrueSig
toTermSig Language.Sprite.Syntax.Inner.Abs.ConstFalse
  = Right ConstFalseSig
toTermSig (Language.Sprite.Syntax.Inner.Abs.PEq _x_aciR _x_aciS)
  = Right (PEqSig _x_aciR _x_aciS)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.PLessOrEqThan _x_aciU _x_aciV)
  = Right (PLessOrEqThanSig _x_aciU _x_aciV)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.PLessThan _x_aciX _x_aciY)
  = Right (PLessThanSig _x_aciX _x_aciY)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Plus _x_acj0 _x_acj1)
  = Right (PlusSig _x_acj0 _x_acj1)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Minus _x_acj3 _x_acj4)
  = Right (MinusSig _x_acj3 _x_acj4)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.Multiply _x_acj6 _x_acj7)
  = Right (MultiplySig _x_acj6 _x_acj7)
toPattern ::
  forall o r_acjf. (Foil.Distinct o,
                    Ord Language.Sprite.Syntax.Inner.Abs.VarIdent) =>
                    Foil.Scope o
                    -> Map.Map Language.Sprite.Syntax.Inner.Abs.VarIdent (Foil.Name o)
                      -> Language.Sprite.Syntax.Inner.Abs.Pattern
                          -> (forall i.
                              Foil.DExt o i =>
                              Pattern o i
                              -> Map.Map Language.Sprite.Syntax.Inner.Abs.VarIdent (Foil.Name i)
                                -> r_acjf)
                            -> r_acjf
toPattern
  _scope_acj8
  _env_acj9
  (Language.Sprite.Syntax.Inner.Abs.PatternVar _x_acjb)
  _cont_acja
  = Foil.withFresh
      _scope_acj8
      (\ _x'_acjc
          -> let
              _scope_acjd = Foil.extendScope _x'_acjc _scope_acj8
              _env_acje
                = Map.insert
                    _x_acjb (Foil.nameOf _x'_acjc) (fmap Foil.sink _env_acj9)
            in _cont_acja (PatternVar _x'_acjc) _env_acje)
toTerm ::
  forall o. (Foil.Distinct o,
              Ord Language.Sprite.Syntax.Inner.Abs.VarIdent) =>
            Foil.Scope o
            -> Map.Map Language.Sprite.Syntax.Inner.Abs.VarIdent (Foil.Name o)
                -> Language.Sprite.Syntax.Inner.Abs.Term -> Term o
toTerm
  = Control.Monad.Free.Foil.convertToAST
      toTermSig toPattern Language.Sprite.FreeFoilConfig.rawScopeToTerm

-- >>> "(x)  => { let x = y; y }" :: Expr Foil.VoidS
-- (x0) =>
-- {
--   let x0 = x1;
--   x1
-- }
instance Show (Term n) where
  -- show = show . fromTerm
  show = Raw.printTree . fromTerm

instance IsString (Term Foil.VoidS) where
  fromString = toTerm Foil.emptyScope Map.empty . unsafeParseTerm
    where
      unsafeParseTerm input =
        case Raw.pTerm (Raw.myLexer input) of
          Left err -> error err
          Right term -> term
