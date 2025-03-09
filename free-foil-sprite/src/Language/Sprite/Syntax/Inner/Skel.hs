-- File generated by the BNF Converter (bnfc 2.9.5).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Language.Sprite.Syntax.Inner.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Language.Sprite.Syntax.Inner.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transVarIdent :: Language.Sprite.Syntax.Inner.Abs.VarIdent -> Result
transVarIdent x = case x of
  Language.Sprite.Syntax.Inner.Abs.VarIdent string -> failure x

transTerm :: Language.Sprite.Syntax.Inner.Abs.Term -> Result
transTerm x = case x of
  Language.Sprite.Syntax.Inner.Abs.ConstInt integer -> failure x
  Language.Sprite.Syntax.Inner.Abs.Boolean constbool -> failure x
  Language.Sprite.Syntax.Inner.Abs.Var varident -> failure x
  Language.Sprite.Syntax.Inner.Abs.If term1 term2 term3 -> failure x
  Language.Sprite.Syntax.Inner.Abs.Let pattern_ term scopedterm -> failure x
  Language.Sprite.Syntax.Inner.Abs.LetRec term pattern_ scopedterm1 scopedterm2 -> failure x
  Language.Sprite.Syntax.Inner.Abs.Fun pattern_ scopedterm -> failure x
  Language.Sprite.Syntax.Inner.Abs.App term1 term2 -> failure x
  Language.Sprite.Syntax.Inner.Abs.Ann term1 term2 -> failure x
  Language.Sprite.Syntax.Inner.Abs.OpExpr term1 op term2 -> failure x
  Language.Sprite.Syntax.Inner.Abs.TAbs pattern_ scopedterm -> failure x
  Language.Sprite.Syntax.Inner.Abs.TApp term1 term2 -> failure x
  Language.Sprite.Syntax.Inner.Abs.TypeRefined term pattern_ scopedterm -> failure x
  Language.Sprite.Syntax.Inner.Abs.TypeRefinedUnknown term -> failure x
  Language.Sprite.Syntax.Inner.Abs.TypeFun pattern_ term scopedterm -> failure x
  Language.Sprite.Syntax.Inner.Abs.TypeForall pattern_ scopedterm -> failure x
  Language.Sprite.Syntax.Inner.Abs.HVar varident terms -> failure x
  Language.Sprite.Syntax.Inner.Abs.BaseTypeInt -> failure x
  Language.Sprite.Syntax.Inner.Abs.BaseTypeBool -> failure x
  Language.Sprite.Syntax.Inner.Abs.BaseTypeVar term -> failure x

transConstBool :: Language.Sprite.Syntax.Inner.Abs.ConstBool -> Result
transConstBool x = case x of
  Language.Sprite.Syntax.Inner.Abs.ConstTrue -> failure x
  Language.Sprite.Syntax.Inner.Abs.ConstFalse -> failure x

transOp :: Language.Sprite.Syntax.Inner.Abs.Op -> Result
transOp x = case x of
  Language.Sprite.Syntax.Inner.Abs.EqOp -> failure x
  Language.Sprite.Syntax.Inner.Abs.LessOrEqOp -> failure x
  Language.Sprite.Syntax.Inner.Abs.LessOp -> failure x
  Language.Sprite.Syntax.Inner.Abs.GreaterOrEqOp -> failure x
  Language.Sprite.Syntax.Inner.Abs.GreaterOp -> failure x
  Language.Sprite.Syntax.Inner.Abs.PlusOp -> failure x
  Language.Sprite.Syntax.Inner.Abs.MinusOp -> failure x
  Language.Sprite.Syntax.Inner.Abs.MultiplyOp -> failure x
  Language.Sprite.Syntax.Inner.Abs.AndOp -> failure x
  Language.Sprite.Syntax.Inner.Abs.OrOp -> failure x

transPattern :: Language.Sprite.Syntax.Inner.Abs.Pattern -> Result
transPattern x = case x of
  Language.Sprite.Syntax.Inner.Abs.PatternVar varident -> failure x

transScopedTerm :: Language.Sprite.Syntax.Inner.Abs.ScopedTerm -> Result
transScopedTerm x = case x of
  Language.Sprite.Syntax.Inner.Abs.ScopedTerm term -> failure x
