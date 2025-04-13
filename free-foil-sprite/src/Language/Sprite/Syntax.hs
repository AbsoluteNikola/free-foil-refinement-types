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
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Language.Refinements.Constraint as Refinements
import qualified Language.Refinements.TypeSignature as Refinements
import qualified Control.Monad.Free.Foil as F

-- There is issue with generating patterns and instances for AST nodes with Pattern and multiple Terms/Scoped terms.
-- Code below just copy paste of generated code via TH and some fixes (marked as FIXED HERE). I hope I will find some time to fix it in free-foil
-- the same issue with mkFreeFoilConversions
-- mkFreeFoil spriteConfig

data Pattern o i
    where
    PatternVar' :: (Foil.NameBinder o i0_aMZ4) -> Pattern o i0_aMZ4
    PatternNoBinders :: Pattern o o
    PatternSomeBinders :: (Foil.NameBinder o i0_a11Mm) ->
                       (Pattern i0_a11Mm i1_a11Mn) ->
                       Pattern o i1_a11Mn

{- Это хак для того чтоб компилятор не ругался на не полный паттерн матчинг
Сделано потому что PatternSomeBinders и PatternNoBinders встречаются строго в одном месте, в switch case.
Пока FrontToInner пишется и руками и контролируется где какой паттерн используется - это безопасно
-}

pattern PatternVar :: Foil.NameBinder i o -> Pattern i o
pattern PatternVar binder = (PatternVar' binder)

{-# COMPLETE PatternVar #-}

data TermSig scope term
    where
    ConstIntSig :: Integer -> TermSig scope term
    BooleanSig :: Language.Sprite.Syntax.Inner.Abs.ConstBool ->
                    TermSig scope term
    ConstructorSig :: Language.Sprite.Syntax.Inner.Abs.ConIdent ->
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
    SwitchSig :: term -> [term] -> TermSig scope term
    CaseAltSig :: Language.Sprite.Syntax.Inner.Abs.ConIdent ->
                      scope ->
                      TermSig scope term
    TLamSig :: scope -> TermSig scope term
    TAppSig :: term -> term -> TermSig scope term
    TypeRefinedSig :: term -> scope -> TermSig scope term
    TypeFunSig :: term -> scope -> TermSig scope term
    TypeForallSig :: scope -> TermSig scope term
    TypeDataSig :: Language.Sprite.Syntax.Inner.Abs.VarIdent ->
                    [term] ->
                    scope ->
                    TermSig scope term
    HVarSig :: Language.Sprite.Syntax.Inner.Abs.VarIdent ->
                [term] ->
                TermSig scope term
    MeasureSig :: Language.Sprite.Syntax.Inner.Abs.MeasureIdent ->
                [term] ->
                TermSig scope term
    UnknownSig :: TermSig scope term
    BaseTypeIntSig :: TermSig scope term
    BaseTypeBoolSig :: TermSig scope term
    BaseTypeVarSig :: term -> TermSig scope term
    BaseTypeTempVarSig :: Language.Sprite.Syntax.Inner.Abs.VarIdent ->
      TermSig scope term
    deriving (GHC.Generics.Generic, Functor, Foldable, Traversable)
type Term = Control.Monad.Free.Foil.AST Pattern TermSig
type ScopedTerm = Control.Monad.Free.Foil.ScopedAST Pattern TermSig
pattern ConstInt :: Integer -> Term o
pattern ConstInt x_aMZ5 = Control.Monad.Free.Foil.Node (ConstIntSig x_aMZ5)
pattern Boolean ::
            Language.Sprite.Syntax.Inner.Abs.ConstBool -> Term o
pattern Boolean x_aMZ6 = Control.Monad.Free.Foil.Node (BooleanSig x_aMZ6)

pattern Constructor :: Language.Sprite.Syntax.Inner.Abs.ConIdent -> Term o
pattern Constructor conId = Control.Monad.Free.Foil.Node (ConstructorSig conId)

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
pattern Switch :: Term o -> [Term o] -> Term o
pattern Switch x_a11MK x_a11ML = Control.Monad.Free.Foil.Node (SwitchSig x_a11MK
                                                                          x_a11ML)
pattern CaseAlt :: Language.Sprite.Syntax.Inner.Abs.ConIdent -> Pattern o i -> Term i -> Term o
pattern CaseAlt x_a11MM binder_a11MN body_a11MO = Control.Monad.Free.Foil.Node (CaseAltSig x_a11MM
                                                                                               (Control.Monad.Free.Foil.ScopedAST binder_a11MN
                                                                                                                                  body_a11MO))
pattern TLam :: Pattern o i -> Term i -> Term o
pattern TLam p term = Control.Monad.Free.Foil.Node (TLamSig (Control.Monad.Free.Foil.ScopedAST p term))
pattern TApp :: Term o -> Term o -> Term o
pattern TApp x_aMZs x_aMZt = Control.Monad.Free.Foil.Node (TAppSig x_aMZs
                                                                    x_aMZt)
pattern TypeRefined :: Term o -> Pattern o i -> Term i -> Term o
pattern TypeRefined x_aMZu binder_aMZv body_aMZw = Control.Monad.Free.Foil.Node (TypeRefinedSig x_aMZu
                                                                                                (Control.Monad.Free.Foil.ScopedAST binder_aMZv
                                                                                                                                    body_aMZw))

pattern TypeFun :: Pattern o i -> Term o -> Term i -> Term o -- FIXED HERE
pattern TypeFun binder_a961 x_a960 body_a962 = Control.Monad.Free.Foil.Node
  (TypeFunSig x_a960
    (Control.Monad.Free.Foil.ScopedAST binder_a961
     body_a962))

pattern TypeForall :: Pattern o i -> Term i -> Term o
pattern TypeForall binder_aMZB body_aMZC = Control.Monad.Free.Foil.Node (TypeForallSig (Control.Monad.Free.Foil.ScopedAST binder_aMZB
                                                                                                                            body_aMZC))
pattern TypeData ::
              Language.Sprite.Syntax.Inner.Abs.VarIdent
              -> [Term i] -> Pattern i o -> Term o -> Term i
pattern TypeData typeId typeArgs refVar refPred = Control.Monad.Free.Foil.Node
  (TypeDataSig typeId typeArgs
    (Control.Monad.Free.Foil.ScopedAST refVar refPred))
pattern HVar ::Language.Sprite.Syntax.Inner.Abs.VarIdent -> [Term o] -> Term o
pattern HVar x_aMZD x_aMZE = Control.Monad.Free.Foil.Node (HVarSig x_aMZD
                                                                    x_aMZE)
pattern Measure ::Language.Sprite.Syntax.Inner.Abs.MeasureIdent -> [Term o] -> Term o
pattern Measure x_aMZD x_aMZE = Control.Monad.Free.Foil.Node (MeasureSig x_aMZD
                                                                    x_aMZE)
pattern Unknown :: Term o
pattern Unknown = Control.Monad.Free.Foil.Node UnknownSig
pattern BaseTypeInt :: Term o
pattern BaseTypeInt = Control.Monad.Free.Foil.Node BaseTypeIntSig
pattern BaseTypeBool :: Term o
pattern BaseTypeBool = Control.Monad.Free.Foil.Node BaseTypeBoolSig
pattern BaseTypeVar :: Term o -> Term o
pattern BaseTypeVar x_aMZF = Control.Monad.Free.Foil.Node (BaseTypeVarSig x_aMZF)
pattern BaseTypeTempVar ::Language.Sprite.Syntax.Inner.Abs.VarIdent -> Term o
pattern BaseTypeTempVar varId = Control.Monad.Free.Foil.Node (BaseTypeTempVarSig varId)
{-# COMPLETE Control.Monad.Free.Foil.Var, Switch, CaseAlt, ConstInt, Boolean, If, Let, LetRec, Fun, App, Ann, OpExpr, TLam, TApp, TypeRefined, TypeFun, TypeForall, TypeData, HVar, BaseTypeInt, BaseTypeBool, BaseTypeVar #-}


deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig

instance Foil.CoSinkable Pattern where
  coSinkabilityProof rename (PatternVar' binder) cont =
    Foil.coSinkabilityProof rename binder $ \rename' binder' ->
      cont rename' (PatternVar' binder')

  coSinkabilityProof rename PatternNoBinders cont = cont rename PatternNoBinders

  coSinkabilityProof rename (PatternSomeBinders binder binders) cont =
    Foil.coSinkabilityProof rename binder $ \rename' binder' ->
      Foil.coSinkabilityProof rename' binders $ \rename'' binders' ->
        cont rename'' (PatternSomeBinders binder' binders')

  withPattern withBinder unit comp scope binders cont = case binders of
    PatternVar' binder ->
      withBinder scope binder $ \f' binder' ->
        cont f' (PatternVar' binder')
    PatternNoBinders -> cont unit PatternNoBinders
    PatternSomeBinders binder moreBinders ->
        Foil.withPattern withBinder unit comp scope binder $ \f binder' ->
          let scope' = Foil.extendScopePattern binder' scope
           in Foil.withPattern withBinder unit comp scope' moreBinders $ \g moreBinders' ->
                cont (comp f g) (PatternSomeBinders binder' moreBinders')


-- mkFreeFoilConversions spriteConfig

fromTermSig ::
  TermSig (Language.Sprite.Syntax.Inner.Abs.Pattern,
            Language.Sprite.Syntax.Inner.Abs.ScopedTerm) Language.Sprite.Syntax.Inner.Abs.Term
  -> Language.Sprite.Syntax.Inner.Abs.Term
fromTermSig (ConstIntSig x_aavY)
  = Language.Sprite.Syntax.Inner.Abs.ConstInt x_aavY
fromTermSig (BooleanSig x_aavZ)
  = Language.Sprite.Syntax.Inner.Abs.Boolean x_aavZ
fromTermSig (ConstructorSig conId)
 = Language.Sprite.Syntax.Inner.Abs.Constructor conId
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
fromTermSig (SwitchSig x_a14NA x_a14NB)
  = Language.Sprite.Syntax.Inner.Abs.Switch x_a14NA x_a14NB
fromTermSig (CaseAltSig x_a14NC (binder_a14ND, body_a14NE))
  = Language.Sprite.Syntax.Inner.Abs.CaseAlt
      x_a14NC binder_a14ND body_a14NE
fromTermSig (TLamSig (binder_aawk, body_aawl))
  = Language.Sprite.Syntax.Inner.Abs.TLam binder_aawk body_aawl
fromTermSig (TAppSig x_aawm x_aawn)
  = Language.Sprite.Syntax.Inner.Abs.TApp x_aawm x_aawn
fromTermSig (TypeRefinedSig x_aawo (binder_aawp, body_aawq))
  = Language.Sprite.Syntax.Inner.Abs.TypeRefined
      x_aawo binder_aawp body_aawq
fromTermSig (TypeFunSig x_abJi (binder_abJj, body_abJk))
  = Language.Sprite.Syntax.Inner.Abs.TypeFun
      binder_abJj x_abJi  body_abJk -- FIXED HERE
fromTermSig (TypeForallSig (binder_aawv, body_aaww))
  = Language.Sprite.Syntax.Inner.Abs.TypeForall binder_aawv body_aaww
fromTermSig (HVarSig x_aawx x_aawy)
  = Language.Sprite.Syntax.Inner.Abs.HVar x_aawx x_aawy
fromTermSig (MeasureSig x_aawx x_aawy)
  = Language.Sprite.Syntax.Inner.Abs.Measure x_aawx x_aawy
fromTermSig BaseTypeIntSig
  = Language.Sprite.Syntax.Inner.Abs.BaseTypeInt
fromTermSig UnknownSig
  = Language.Sprite.Syntax.Inner.Abs.Unknown
fromTermSig BaseTypeBoolSig
  = Language.Sprite.Syntax.Inner.Abs.BaseTypeBool
fromTermSig (BaseTypeVarSig x_aawz)
  = Language.Sprite.Syntax.Inner.Abs.BaseTypeVar x_aawz
fromTermSig (BaseTypeTempVarSig varId)
  = Language.Sprite.Syntax.Inner.Abs.BaseTypeTempVar varId
fromTermSig (TypeDataSig varId typArgs (refVar, refPred))
  = Language.Sprite.Syntax.Inner.Abs.TypeData varId (toTypArgs typArgs) refVar refPred
  where
    toTypArgs [] = Language.Sprite.Syntax.Inner.Abs.EmptyTypeDataArgs
    toTypArgs xs = Language.Sprite.Syntax.Inner.Abs.NonEmptyTypeDataArgs
      $ Language.Sprite.Syntax.Inner.Abs.TypeDataArg <$> xs

fromPattern ::
      Pattern o i -> Language.Sprite.Syntax.Inner.Abs.Pattern
fromPattern (PatternVar' x_aceP)
  = Language.Sprite.Syntax.Inner.Abs.PatternVar
      (intToVarIdent (Foil.nameId (Foil.nameOf x_aceP)))
fromPattern PatternNoBinders
  = Language.Sprite.Syntax.Inner.Abs.PatternNoBinders
fromPattern (PatternSomeBinders x_aceQ x_aceR)
  = Language.Sprite.Syntax.Inner.Abs.PatternSomeBinders
      (intToVarIdent (Foil.nameId (Foil.nameOf x_aceQ)))
      (fromPattern x_aceR)

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
toTermSig (Language.Sprite.Syntax.Inner.Abs.Constructor conId)
  = Right (ConstructorSig conId)
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
  (Language.Sprite.Syntax.Inner.Abs.Switch _x_a14Ox _x_a14Oy)
  = Right (SwitchSig _x_a14Ox _x_a14Oy)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.CaseAlt _x_a14OA binder_a14OB
                                            body_a14OC)
  = Right (CaseAltSig _x_a14OA (binder_a14OB, body_a14OC))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TLam binder_aax8 body_aax9)
  = Right (TLamSig (binder_aax8, body_aax9))
toTermSig (Language.Sprite.Syntax.Inner.Abs.TApp _x_aaxb _x_aaxc)
  = Right (TAppSig _x_aaxb _x_aaxc)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeRefined _x_aaxe binder_aaxf
                                                body_aaxg)
  = Right (TypeRefinedSig _x_aaxe (binder_aaxf, body_aaxg))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeFun binder_abJM _x_abJL -- FIXED HERE
                                            body_abJN)
  = Right (TypeFunSig _x_abJL (binder_abJM, body_abJN))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeForall binder_aaxo body_aaxp)
  = Right (TypeForallSig (binder_aaxo, body_aaxp))
toTermSig (Language.Sprite.Syntax.Inner.Abs.HVar _x_aaxr _x_aaxs)
  = Right (HVarSig _x_aaxr _x_aaxs)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Measure _x_aaxr _x_aaxs)
  = Right (MeasureSig _x_aaxr _x_aaxs)
toTermSig Language.Sprite.Syntax.Inner.Abs.Unknown
  = Right UnknownSig
toTermSig Language.Sprite.Syntax.Inner.Abs.BaseTypeInt
  = Right BaseTypeIntSig
toTermSig Language.Sprite.Syntax.Inner.Abs.BaseTypeBool
  = Right BaseTypeBoolSig
toTermSig (Language.Sprite.Syntax.Inner.Abs.BaseTypeVar _x_aaxw)
  = Right (BaseTypeVarSig _x_aaxw)
toTermSig (Language.Sprite.Syntax.Inner.Abs.BaseTypeTempVar varId)
  = Right (BaseTypeTempVarSig varId)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeData typId typArgs refVar refPred)
  = Right (TypeDataSig typId (toTypArgs typArgs) (refVar, refPred))
  where
    toTypArgs Language.Sprite.Syntax.Inner.Abs.EmptyTypeDataArgs = []
    toTypArgs (Language.Sprite.Syntax.Inner.Abs.NonEmptyTypeDataArgs args) = flip map args
      $ \(Language.Sprite.Syntax.Inner.Abs.TypeDataArg arg) -> arg
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
  _scope_acfZ
  _env_acg0
  (Language.Sprite.Syntax.Inner.Abs.PatternVar _x_acg2)
  _cont_acg1
  = Foil.withFresh
      _scope_acfZ
      (\ _x'_acg3
          -> let
              _scope_acg4 = Foil.extendScope _x'_acg3 _scope_acfZ
              _env_acg5
                = Map.insert
                    _x_acg2 (Foil.nameOf _x'_acg3) (fmap Foil.sink _env_acg0)
            in _cont_acg1 (PatternVar _x'_acg3) _env_acg5)
toPattern
  _scope_acg6
  _env_acg7
  Language.Sprite.Syntax.Inner.Abs.PatternNoBinders
  _cont_acg8
  = _cont_acg8 PatternNoBinders _env_acg7
toPattern
  _scope_acg9
  _env_acga
  (Language.Sprite.Syntax.Inner.Abs.PatternSomeBinders _x_acgc
                                                        _x_acgg)
  _cont_acgb
  = Foil.withFresh
      _scope_acg9
      (\ _x'_acgd
          -> let
              _scope_acge = Foil.extendScope _x'_acgd _scope_acg9
              _env_acgf
                = Map.insert
                    _x_acgc (Foil.nameOf _x'_acgd) (fmap Foil.sink _env_acga)
            in
              toPattern
                _scope_acge _env_acgf _x_acgg
                (\ _x'_acgh _env_acgj
                    -> let _scope_acgi = Foil.extendScopePattern _x'_acgh _scope_acge
                      in _cont_acgb (PatternSomeBinders _x'_acgd _x'_acgh) _env_acgj))

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

instance Refinements.IsType TermSig Pattern where
  withPred (TypeRefined base (PatternVar v) p) f = case (Foil.assertDistinct v, Foil.assertExt v) of
    (Foil.Distinct, Foil.Ext) -> case f $ Refinements.WithPred v p of
      Refinements.WithPred v' p' -> TypeRefined base (PatternVar v') p'
  withPred (TypeData name args (PatternVar v) p) f = case (Foil.assertDistinct v, Foil.assertExt v) of
    (Foil.Distinct, Foil.Ext) ->
      case f $ Refinements.WithPred v p of
      Refinements.WithPred v' p' -> TypeData name args (PatternVar v') p'
  withPred t _ = t

  toTypeSignature (TypeRefined BaseTypeInt _ _) = Refinements.IntType
  toTypeSignature (TypeRefined BaseTypeBool _ _) = Refinements.BoolType
  toTypeSignature (TypeRefined (F.Var v) _ _) = Refinements.VarType (varToPredId v )
  toTypeSignature
    (TypeRefined (BaseTypeTempVar v) _ _)
    = Refinements.VarType (Refinements.Id $ getRawVarId v)
  toTypeSignature (TypeData name args _ _) = Refinements.DataType name' args'
    where
      name' = Refinements.Id $ getRawVarId name
      args' = map (Refinements.DataTypeArg . Refinements.toTypeSignature) args
  toTypeSignature (TypeFun _ argT retT) = Refinements.FunType
    (Refinements.toTypeSignature argT)
    (Refinements.toTypeSignature retT)
  toTypeSignature t = error $ "Unknown type: " <> show t

getRawVarId :: Language.Sprite.Syntax.Inner.Abs.VarIdent -> String
getRawVarId (Language.Sprite.Syntax.Inner.Abs.VarIdent v) = v

varToPredId :: Foil.Name o -> Refinements.Id
varToPredId name =
  let (Language.Sprite.Syntax.Inner.Abs.VarIdent v) = intToVarIdent . Foil.nameId $ name
  in Refinements.Id v

instance Refinements.IsPred TermSig Pattern where
  isUnknown Unknown = True
  isUnknown _ = False

  mkAnd l r= OpExpr l Language.Sprite.Syntax.Inner.Abs.AndOp r
  mkEq l r= OpExpr l Language.Sprite.Syntax.Inner.Abs.EqOp r

  toPredicate = undefined -- TODO
