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
    PatternVar :: (Foil.NameBinder o i0_a950) -> Pattern o i0_a950
data TermSig scope term
  where
    ConstIntSig :: Integer -> TermSig scope term
    LetSig :: term -> scope -> TermSig scope term
    FunSig :: scope -> TermSig scope term
    AppSig :: term -> term -> TermSig scope term
    AnnSig :: term -> term -> TermSig scope term
    OpExprSig :: term ->
                  Language.Sprite.Syntax.Inner.Abs.Op ->
                  term ->
                  TermSig scope term
    TypeRefinedSig :: Language.Sprite.Syntax.Inner.Abs.BaseType ->
                      scope ->
                      TermSig scope term
    TypeFunSig :: term -> scope -> TermSig scope term
    ConstTrueSig :: TermSig scope term
    ConstFalseSig :: TermSig scope term
  deriving (GHC.Generics.Generic, Functor, Foldable, Traversable)

type Term = Control.Monad.Free.Foil.AST Pattern TermSig
type ScopedTerm = Control.Monad.Free.Foil.ScopedAST Pattern TermSig
pattern ConstInt :: Integer -> Term o
pattern ConstInt x_a95K = Control.Monad.Free.Foil.Node (ConstIntSig x_a95K)
pattern Let :: Pattern o i -> Term o -> Term i -> Term o -- FIXED HERE
pattern Let binder_a95M x_a95L body_a95N = Control.Monad.Free.Foil.Node (LetSig x_a95L
                                                                                (Control.Monad.Free.Foil.ScopedAST binder_a95M
                                                                                                                    body_a95N))
pattern Fun :: Pattern o i -> Term i -> Term o
pattern Fun binder_a95O body_a95P = Control.Monad.Free.Foil.Node (FunSig (Control.Monad.Free.Foil.ScopedAST binder_a95O
                                                                                                            body_a95P))
pattern App :: Term o -> Term o -> Term o
pattern App x_a95Q x_a95R = Control.Monad.Free.Foil.Node (AppSig x_a95Q
                                                                  x_a95R)
pattern Ann :: Term o -> Term o -> Term o
pattern Ann x_a95S x_a95T = Control.Monad.Free.Foil.Node (AnnSig x_a95S
                                                                  x_a95T)
pattern OpExpr ::
          Term o -> Language.Sprite.Syntax.Inner.Abs.Op -> Term o -> Term o
pattern OpExpr x_a95U x_a95V x_a95W = Control.Monad.Free.Foil.Node (OpExprSig x_a95U
                                                                              x_a95V x_a95W)
pattern TypeRefined ::
          Language.Sprite.Syntax.Inner.Abs.BaseType
          -> Pattern o i -> Term i -> Term o
pattern TypeRefined x_a95X binder_a95Y body_a95Z = Control.Monad.Free.Foil.Node (TypeRefinedSig x_a95X
                                                                                                (Control.Monad.Free.Foil.ScopedAST binder_a95Y
                                                                                                                                    body_a95Z))
pattern TypeFun :: Pattern o i -> Term o -> Term i -> Term o -- FIXED HERE
pattern TypeFun binder_a961 x_a960 body_a962 = Control.Monad.Free.Foil.Node (TypeFunSig x_a960
                                                                                        (Control.Monad.Free.Foil.ScopedAST binder_a961
                                                                                                                            body_a962))
pattern ConstTrue :: Term o
pattern ConstTrue = Control.Monad.Free.Foil.Node ConstTrueSig
pattern ConstFalse :: Term o
pattern ConstFalse = Control.Monad.Free.Foil.Node ConstFalseSig
{-# COMPLETE Control.Monad.Free.Foil.Var, ConstInt, Let, Fun, App, Ann, OpExpr, TypeRefined, TypeFun, ConstTrue, ConstFalse #-}

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
fromTermSig (ConstIntSig x_abJ2)
  = Language.Sprite.Syntax.Inner.Abs.ConstInt x_abJ2
fromTermSig (LetSig x_abJ3 (binder_abJ4, body_abJ5))
  = Language.Sprite.Syntax.Inner.Abs.Let binder_abJ4 x_abJ3 body_abJ5 -- FIXED HERE
fromTermSig (FunSig (binder_abJ6, body_abJ7))
  = Language.Sprite.Syntax.Inner.Abs.Fun binder_abJ6 body_abJ7
fromTermSig (AppSig x_abJ8 x_abJ9)
  = Language.Sprite.Syntax.Inner.Abs.App x_abJ8 x_abJ9
fromTermSig (AnnSig x_abJa x_abJb)
  = Language.Sprite.Syntax.Inner.Abs.Ann x_abJa x_abJb
fromTermSig (OpExprSig x_abJc x_abJd x_abJe)
  = Language.Sprite.Syntax.Inner.Abs.OpExpr x_abJc x_abJd x_abJe
fromTermSig (TypeRefinedSig x_abJf (binder_abJg, body_abJh))
  = Language.Sprite.Syntax.Inner.Abs.TypeRefined
      x_abJf binder_abJg body_abJh
fromTermSig (TypeFunSig x_abJi (binder_abJj, body_abJk))
  = Language.Sprite.Syntax.Inner.Abs.TypeFun
      binder_abJj x_abJi  body_abJk -- FIXED HERE
fromTermSig ConstTrueSig
  = Language.Sprite.Syntax.Inner.Abs.ConstTrue
fromTermSig ConstFalseSig
  = Language.Sprite.Syntax.Inner.Abs.ConstFalse
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
toTermSig (Language.Sprite.Syntax.Inner.Abs.ConstInt _x_abJn)
  = Right (ConstIntSig _x_abJn)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Var _theRawIdent_abJo)
  = Left _theRawIdent_abJo
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.Let binder_abJr _x_abJq -- FIXED HERE
                                        body_abJs)
  = Right (LetSig _x_abJq (binder_abJr, body_abJs))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.Fun binder_abJu body_abJv)
  = Right (FunSig (binder_abJu, body_abJv))
toTermSig (Language.Sprite.Syntax.Inner.Abs.App _x_abJx _x_abJy)
  = Right (AppSig _x_abJx _x_abJy)
toTermSig (Language.Sprite.Syntax.Inner.Abs.Ann _x_abJA _x_abJB)
  = Right (AnnSig _x_abJA _x_abJB)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.OpExpr _x_abJD _x_abJE _x_abJF)
  = Right (OpExprSig _x_abJD _x_abJE _x_abJF)
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeRefined _x_abJH binder_abJI
                                                body_abJJ)
  = Right (TypeRefinedSig _x_abJH (binder_abJI, body_abJJ))
toTermSig
  (Language.Sprite.Syntax.Inner.Abs.TypeFun binder_abJM _x_abJL -- FIXED HERE
                                            body_abJN)
  = Right (TypeFunSig _x_abJL (binder_abJM, body_abJN))
toTermSig Language.Sprite.Syntax.Inner.Abs.ConstTrue
  = Right ConstTrueSig
toTermSig Language.Sprite.Syntax.Inner.Abs.ConstFalse
  = Right ConstFalseSig
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
