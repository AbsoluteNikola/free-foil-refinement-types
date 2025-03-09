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
{-# LANGUAGE InstanceSigs #-}
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

-- There is issue with generating patterns and instances for AST nodes with Pattern and multiple Terms/Scoped terms.
-- Code below just copy paste of generated code via TH and some fixes (marked as FIXED HERE). I hope I will find some time to fix it in free-foil
-- the same issue with mkFreeFoilConversions
-- mkFreeFoil spriteConfig

data Pattern o i
    where
    PatternVar :: (Foil.NameBinder o i0_aMZ4) -> Pattern o i0_aMZ4
data TermSig scope term
    where
    ConstIntSig :: Integer -> TermSig scope term
    BooleanSig :: Language.Sprite.Syntax.Inner.Abs.ConstBool ->
                    TermSig scope term
    IfSig :: term -> term -> term -> TermSig scope term
    LetSig :: term -> scope -> TermSig scope term
    LetRecSig :: term -> scope -> scope -> TermSig scope term
    FunSig :: scope -> TermSig scope term
    AppSig :: term -> term -> TermSig scope term
    AnnSig :: term -> term -> TermSig scope term
    OpExprSig :: term ->
                    Language.Sprite.Syntax.Inner.Abs.Op ->
                    term ->
                    TermSig scope term
    TAbsSig :: scope -> TermSig scope term
    TAppSig :: term -> term -> TermSig scope term
    TypeRefinedSig :: term -> scope -> TermSig scope term
    TypeRefinedUnknownSig :: term -> TermSig scope term
    TypeFunSig :: term -> scope -> TermSig scope term
    TypeForallSig :: scope -> TermSig scope term
    HVarSig :: Language.Sprite.Syntax.Inner.Abs.VarIdent ->
                [term] ->
                TermSig scope term
    BaseTypeIntSig :: TermSig scope term
    BaseTypeBoolSig :: TermSig scope term
    BaseTypeVarSig :: term -> TermSig scope term
    deriving (GHC.Generics.Generic, Functor, Foldable, Traversable)
type Term = Control.Monad.Free.Foil.AST Pattern TermSig
type ScopedTerm = Control.Monad.Free.Foil.ScopedAST Pattern TermSig
pattern ConstInt :: Integer -> Term o
pattern ConstInt x_aMZ5 = Control.Monad.Free.Foil.Node (ConstIntSig x_aMZ5)
pattern Boolean ::
            Language.Sprite.Syntax.Inner.Abs.ConstBool -> Term o
pattern Boolean x_aMZ6 = Control.Monad.Free.Foil.Node (BooleanSig x_aMZ6)
pattern If :: Term o -> Term o -> Term o -> Term o
pattern If cond thenB elseB = Control.Monad.Free.Foil.Node (IfSig cond thenB elseB)

pattern Let :: Pattern o i -> Term o -> Term i -> Term o -- FIXED HERE
pattern Let binder_a95M x_a95L body_a95N = Control.Monad.Free.Foil.Node
  (LetSig x_a95L
    (Control.Monad.Free.Foil.ScopedAST binder_a95M
                                        body_a95N))

pattern LetRec :: Term o -> Pattern o i1 -> Pattern o i2 -> Term i1 -> Term i2 -> Term o -- FIXED HERE
pattern LetRec typ binder_1 binder_2 x_a95L body_a95N = Control.Monad.Free.Foil.Node (LetRecSig
    typ
    (Control.Monad.Free.Foil.ScopedAST binder_1 x_a95L)
    (Control.Monad.Free.Foil.ScopedAST binder_2 body_a95N))

pattern Fun :: Pattern o i -> Term i -> Term o
pattern Fun binder_aMZi body_aMZj = Control.Monad.Free.Foil.Node (FunSig (Control.Monad.Free.Foil.ScopedAST binder_aMZi
                                                                                                            body_aMZj))
pattern App :: Term o -> Term o -> Term o
pattern App x_aMZk x_aMZl = Control.Monad.Free.Foil.Node (AppSig x_aMZk
                                                                    x_aMZl)
pattern Ann :: Term o -> Term o -> Term o
pattern Ann x_aMZm x_aMZn = Control.Monad.Free.Foil.Node (AnnSig x_aMZm
                                                                    x_aMZn)
pattern OpExpr ::
            Term o -> Language.Sprite.Syntax.Inner.Abs.Op -> Term o -> Term o
pattern OpExpr x_aMZo x_aMZp x_aMZq = Control.Monad.Free.Foil.Node (OpExprSig x_aMZo
                                                                                x_aMZp x_aMZq)
pattern TAbs :: Pattern o i -> Term i -> Term o
pattern TAbs p term = Control.Monad.Free.Foil.Node (TAbsSig (Control.Monad.Free.Foil.ScopedAST p term))
pattern TApp :: Term o -> Term o -> Term o
pattern TApp x_aMZs x_aMZt = Control.Monad.Free.Foil.Node (TAppSig x_aMZs
                                                                    x_aMZt)
pattern TypeRefined :: Term o -> Pattern o i -> Term i -> Term o
pattern TypeRefined x_aMZu binder_aMZv body_aMZw = Control.Monad.Free.Foil.Node (TypeRefinedSig x_aMZu
                                                                                                (Control.Monad.Free.Foil.ScopedAST binder_aMZv
                                                                                                                                    body_aMZw))
pattern TypeRefinedUnknown :: Term o -> Term o
pattern TypeRefinedUnknown x_aMZx = Control.Monad.Free.Foil.Node (TypeRefinedUnknownSig x_aMZx)

pattern TypeFun :: Pattern o i -> Term o -> Term i -> Term o -- FIXED HERE
pattern TypeFun binder_a961 x_a960 body_a962 = Control.Monad.Free.Foil.Node
  (TypeFunSig x_a960
    (Control.Monad.Free.Foil.ScopedAST binder_a961
     body_a962))

pattern TypeForall :: Pattern o i -> Term i -> Term o
pattern TypeForall binder_aMZB body_aMZC = Control.Monad.Free.Foil.Node (TypeForallSig (Control.Monad.Free.Foil.ScopedAST binder_aMZB
                                                                                                                            body_aMZC))
pattern HVar ::Language.Sprite.Syntax.Inner.Abs.VarIdent -> [Term o] -> Term o
pattern HVar x_aMZD x_aMZE = Control.Monad.Free.Foil.Node (HVarSig x_aMZD
                                                                    x_aMZE)
pattern BaseTypeInt :: Term o
pattern BaseTypeInt = Control.Monad.Free.Foil.Node BaseTypeIntSig
pattern BaseTypeBool :: Term o
pattern BaseTypeBool = Control.Monad.Free.Foil.Node BaseTypeBoolSig
pattern BaseTypeVar :: Term o -> Term o
pattern BaseTypeVar x_aMZF = Control.Monad.Free.Foil.Node (BaseTypeVarSig x_aMZF)
{-# COMPLETE Control.Monad.Free.Foil.Var, ConstInt, Boolean, If, Let, LetRec, Fun, App, Ann, OpExpr, TAbs, TApp, TypeRefined, TypeRefinedUnknown, TypeFun, TypeForall, HVar, BaseTypeInt, BaseTypeBool, BaseTypeVar #-}


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


fromTermSig ::
  TermSig (Language.Sprite.Syntax.Inner.Abs.Pattern,
            Language.Sprite.Syntax.Inner.Abs.ScopedTerm) Language.Sprite.Syntax.Inner.Abs.Term
  -> Language.Sprite.Syntax.Inner.Abs.Term
fromTermSig (ConstIntSig x_aavY)
  = Language.Sprite.Syntax.Inner.Abs.ConstInt x_aavY
fromTermSig (BooleanSig x_aavZ)
  = Language.Sprite.Syntax.Inner.Abs.Boolean x_aavZ
fromTermSig (IfSig x_aaw0 x_aaw1 x_aaw2)
  = Language.Sprite.Syntax.Inner.Abs.If x_aaw0 x_aaw1 x_aaw2
fromTermSig (LetSig x_abJ3 (binder_abJ4, body_abJ5))
  = Language.Sprite.Syntax.Inner.Abs.Let binder_abJ4 x_abJ3 body_abJ5 -- FIXED HERE
fromTermSig (LetRecSig typ (binder_1, x_abJ3) (_, body_abJ5))
  = Language.Sprite.Syntax.Inner.Abs.LetRec typ binder_1 x_abJ3 body_abJ5 -- FIXED HERE
fromTermSig (FunSig (binder_aawb, body_aawc))
  = Language.Sprite.Syntax.Inner.Abs.Fun binder_aawb body_aawc
fromTermSig (AppSig x_aawd x_aawe)
  = Language.Sprite.Syntax.Inner.Abs.App x_aawd x_aawe
fromTermSig (AnnSig x_aawf x_aawg)
  = Language.Sprite.Syntax.Inner.Abs.Ann x_aawf x_aawg
fromTermSig (OpExprSig x_aawh x_aawi x_aawj)
  = Language.Sprite.Syntax.Inner.Abs.OpExpr x_aawh x_aawi x_aawj
fromTermSig (TAbsSig (binder_aawk, body_aawl))
  = Language.Sprite.Syntax.Inner.Abs.TAbs binder_aawk body_aawl
fromTermSig (TAppSig x_aawm x_aawn)
  = Language.Sprite.Syntax.Inner.Abs.TApp x_aawm x_aawn
fromTermSig (TypeRefinedSig x_aawo (binder_aawp, body_aawq))
  = Language.Sprite.Syntax.Inner.Abs.TypeRefined
      x_aawo binder_aawp body_aawq
fromTermSig (TypeRefinedUnknownSig x_aawr)
  = Language.Sprite.Syntax.Inner.Abs.TypeRefinedUnknown x_aawr
fromTermSig (TypeFunSig x_abJi (binder_abJj, body_abJk))
  = Language.Sprite.Syntax.Inner.Abs.TypeFun
      binder_abJj x_abJi  body_abJk -- FIXED HERE
fromTermSig (TypeForallSig (binder_aawv, body_aaww))
  = Language.Sprite.Syntax.Inner.Abs.TypeForall binder_aawv body_aaww
fromTermSig (HVarSig x_aawx x_aawy)
  = Language.Sprite.Syntax.Inner.Abs.HVar x_aawx x_aawy
fromTermSig BaseTypeIntSig
  = Language.Sprite.Syntax.Inner.Abs.BaseTypeInt
fromTermSig BaseTypeBoolSig
  = Language.Sprite.Syntax.Inner.Abs.BaseTypeBool
fromTermSig (BaseTypeVarSig x_aawz)
  = Language.Sprite.Syntax.Inner.Abs.BaseTypeVar x_aawz


fromPattern ::
  Pattern o i -> Language.Sprite.Syntax.Inner.Abs.Pattern
fromPattern (PatternVar x_abJl)
  = Language.Sprite.Syntax.Inner.Abs.PatternVar
      (intToVarIdent (Foil.nameId (Foil.nameOf x_abJl)))
fromTerm :: Term o -> Language.Sprite.Syntax.Inner.Abs.Term
fromTerm
  = Control.Monad.Free.Foil.convertFromAST
      fromTermSig rawVar fromPattern rawScopedTerm intToVarIdent
toTermSig ::
  Language.Sprite.Syntax.Inner.Abs.Term
  -> Either Language.Sprite.Syntax.Inner.Abs.VarIdent (TermSig (Language.Sprite.Syntax.Inner.Abs.Pattern,
                                                                Language.Sprite.Syntax.Inner.Abs.ScopedTerm) Language.Sprite.Syntax.Inner.Abs.Term)
toTermSig (Language.Sprite.Syntax.Inner.Abs.ConstInt _x_aawC)
  = Right (ConstIntSig _x_aawC)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Boolean _x_aawE)
  = Right (BooleanSig _x_aawE)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Var _theRawIdent_aawF)
  = Left _theRawIdent_aawF
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.If _x_aawH _x_aawI _x_aawJ)
  = Right (IfSig _x_aawH _x_aawI _x_aawJ)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.Let binder_abJr _x_abJq -- FIXED HERE
                                        body_abJs)
  = Right (LetSig _x_abJq (binder_abJr, body_abJs))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.LetRec typ binder_abJr _x_abJq -- FIXED HERE
                                        body_abJs)
  = Right (LetRecSig typ (binder_abJr, _x_abJq) (binder_abJr, body_abJs))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.Fun binder_aawV body_aawW)
  = Right (FunSig (binder_aawV, body_aawW))
toTermSig (Language.Sprite.Syntax.Inner.Abs.App _x_aawY _x_aawZ)
  = Right (AppSig _x_aawY _x_aawZ)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Ann _x_aax1 _x_aax2)
  = Right (AnnSig _x_aax1 _x_aax2)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.OpExpr _x_aax4 _x_aax5 _x_aax6)
  = Right (OpExprSig _x_aax4 _x_aax5 _x_aax6)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TAbs binder_aax8 body_aax9)
  = Right (TAbsSig (binder_aax8, body_aax9))
toTermSig (Language.Sprite.Syntax.Inner.Abs.TApp _x_aaxb _x_aaxc)
  = Right (TAppSig _x_aaxb _x_aaxc)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeRefined _x_aaxe binder_aaxf
                                                body_aaxg)
  = Right (TypeRefinedSig _x_aaxe (binder_aaxf, body_aaxg))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeRefinedUnknown _x_aaxi)
  = Right (TypeRefinedUnknownSig _x_aaxi)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeFun binder_abJM _x_abJL -- FIXED HERE
                                            body_abJN)
  = Right (TypeFunSig _x_abJL (binder_abJM, body_abJN))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeForall binder_aaxo body_aaxp)
  = Right (TypeForallSig (binder_aaxo, body_aaxp))
toTermSig (Language.Sprite.Syntax.Inner.Abs.HVar _x_aaxr _x_aaxs)
  = Right (HVarSig _x_aaxr _x_aaxs)
toTermSig Language.Sprite.Syntax.Inner.Abs.BaseTypeInt
  = Right BaseTypeIntSig
toTermSig Language.Sprite.Syntax.Inner.Abs.BaseTypeBool
  = Right BaseTypeBoolSig
toTermSig (Language.Sprite.Syntax.Inner.Abs.BaseTypeVar _x_aaxw)
  = Right (BaseTypeVarSig _x_aaxw)

toPattern ::
  forall o r_abJX. (Foil.Distinct o,
                    Ord Language.Sprite.Syntax.Inner.Abs.VarIdent) =>
                    Foil.Scope o
                    -> Map.Map Language.Sprite.Syntax.Inner.Abs.VarIdent (Foil.Name o)
                      -> Language.Sprite.Syntax.Inner.Abs.Pattern
                          -> (forall i.
                              Foil.DExt o i =>
                              Pattern o i
                              -> Map.Map Language.Sprite.Syntax.Inner.Abs.VarIdent (Foil.Name i)
                                -> r_abJX)
                            -> r_abJX
toPattern
  _scope_abJQ
  _env_abJR
  (Language.Sprite.Syntax.Inner.Abs.PatternVar _x_abJT)
  _cont_abJS
  = Foil.withFresh
      _scope_abJQ
      (\ _x'_abJU
          -> let
              _scope_abJV = Foil.extendScope _x'_abJU _scope_abJQ
              _env_abJW
                = Map.insert
                    _x_abJT (Foil.nameOf _x'_abJU) (fmap Foil.sink _env_abJR)
            in _cont_abJS (PatternVar _x'_abJU) _env_abJW)
toTerm ::
  forall o. (Foil.Distinct o,
              Ord Language.Sprite.Syntax.Inner.Abs.VarIdent) =>
            Foil.Scope o
            -> Map.Map Language.Sprite.Syntax.Inner.Abs.VarIdent (Foil.Name o)
                -> Language.Sprite.Syntax.Inner.Abs.Term -> Term o
toTerm
  = Control.Monad.Free.Foil.convertToAST
      toTermSig toPattern rawScopeToTerm

-- >>> "(x)  => { let x = y; y }" :: Expr Foil.VoidS
-- (x0) =>
-- {
--   let x0 = x1;
--   x1
-- }
instance Show (Term n) where
  -- show = show . fromTerm
  show = Raw.printTree . fromTerm

instance Show (Pattern i o) where
  -- show = show . fromTerm
  show = Raw.printTree . fromPattern

instance IsString (Term Foil.VoidS) where
  fromString :: String -> Term Foil.VoidS
  fromString = toTerm Foil.emptyScope Map.empty . unsafeParseTerm
    where
      unsafeParseTerm input =
        case Raw.pTerm (Raw.myLexer input) of
          Left err -> error err
          Right term -> term

instance Foil.UnifiablePattern Pattern where
 unifyPatterns (PatternVar x) (PatternVar y) = Foil.unifyNameBinders x y
