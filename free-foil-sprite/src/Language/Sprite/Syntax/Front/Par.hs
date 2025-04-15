{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PartialTypeSignatures #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sprite.Syntax.Front.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Language.Sprite.Syntax.Front.Abs
import Language.Sprite.Syntax.Front.Lex
import qualified Data.Function as Happy_Prelude
import qualified Data.Bool as Happy_Prelude
import qualified Data.Function as Happy_Prelude
import qualified Data.Maybe as Happy_Prelude
import qualified Data.Int as Happy_Prelude
import qualified Data.String as Happy_Prelude
import qualified Data.List as Happy_Prelude
import qualified Control.Monad as Happy_Prelude
import qualified Text.Show as Happy_Prelude
import qualified GHC.Num as Happy_Prelude
import qualified GHC.Err as Happy_Prelude
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.1.4

data HappyAbsSyn 
        = HappyTerminal (Token)
        | HappyErrorToken Happy_Prelude.Int
        | HappyAbsSyn5 (Integer)
        | HappyAbsSyn6 (Language.Sprite.Syntax.Front.Abs.VarIdent)
        | HappyAbsSyn7 (Language.Sprite.Syntax.Front.Abs.ConIdent)
        | HappyAbsSyn8 (Language.Sprite.Syntax.Front.Abs.Program)
        | HappyAbsSyn9 (Language.Sprite.Syntax.Front.Abs.Qualifier)
        | HappyAbsSyn10 ([Language.Sprite.Syntax.Front.Abs.Qualifier])
        | HappyAbsSyn11 (Language.Sprite.Syntax.Front.Abs.QualifierArg)
        | HappyAbsSyn12 ([Language.Sprite.Syntax.Front.Abs.QualifierArg])
        | HappyAbsSyn13 (Language.Sprite.Syntax.Front.Abs.Measure)
        | HappyAbsSyn14 ([Language.Sprite.Syntax.Front.Abs.Measure])
        | HappyAbsSyn15 (Language.Sprite.Syntax.Front.Abs.MeasureIdent)
        | HappyAbsSyn16 (Language.Sprite.Syntax.Front.Abs.Term)
        | HappyAbsSyn17 (Language.Sprite.Syntax.Front.Abs.ConstBool)
        | HappyAbsSyn18 (Language.Sprite.Syntax.Front.Abs.ConAppArgs)
        | HappyAbsSyn19 (Language.Sprite.Syntax.Front.Abs.Annotation)
        | HappyAbsSyn20 (Language.Sprite.Syntax.Front.Abs.Decl)
        | HappyAbsSyn21 (Language.Sprite.Syntax.Front.Abs.SwitchCase)
        | HappyAbsSyn22 ([Language.Sprite.Syntax.Front.Abs.SwitchCase])
        | HappyAbsSyn23 (Language.Sprite.Syntax.Front.Abs.SwitchCaseDataConArgs)
        | HappyAbsSyn24 (Language.Sprite.Syntax.Front.Abs.IntOp)
        | HappyAbsSyn25 (Language.Sprite.Syntax.Front.Abs.Refinement)
        | HappyAbsSyn26 (Language.Sprite.Syntax.Front.Abs.RType)
        | HappyAbsSyn27 (Language.Sprite.Syntax.Front.Abs.TypeDataArg)
        | HappyAbsSyn28 ([Language.Sprite.Syntax.Front.Abs.TypeDataArg])
        | HappyAbsSyn29 (Language.Sprite.Syntax.Front.Abs.TypeDataArgs)
        | HappyAbsSyn30 (Language.Sprite.Syntax.Front.Abs.DataType)
        | HappyAbsSyn31 ([Language.Sprite.Syntax.Front.Abs.DataType])
        | HappyAbsSyn32 (Language.Sprite.Syntax.Front.Abs.DataTypeConstructor)
        | HappyAbsSyn33 ([Language.Sprite.Syntax.Front.Abs.DataTypeConstructor])
        | HappyAbsSyn34 (Language.Sprite.Syntax.Front.Abs.DataTypeConstructorArgs)
        | HappyAbsSyn35 (Language.Sprite.Syntax.Front.Abs.DataTypeConstructorPredicate)
        | HappyAbsSyn36 (Language.Sprite.Syntax.Front.Abs.DataTypeArgs)
        | HappyAbsSyn37 (Language.Sprite.Syntax.Front.Abs.FuncArg)
        | HappyAbsSyn38 ([Language.Sprite.Syntax.Front.Abs.FuncArg])
        | HappyAbsSyn39 (Language.Sprite.Syntax.Front.Abs.Pred)
        | HappyAbsSyn46 (Language.Sprite.Syntax.Front.Abs.MeasureArg)
        | HappyAbsSyn47 ([Language.Sprite.Syntax.Front.Abs.MeasureArg])
        | HappyAbsSyn48 (Language.Sprite.Syntax.Front.Abs.BaseType)
        | HappyAbsSyn49 (Language.Sprite.Syntax.Front.Abs.TypeVarId)
        | HappyAbsSyn50 ([Language.Sprite.Syntax.Front.Abs.TypeVarId])
        | HappyAbsSyn51 (Language.Sprite.Syntax.Front.Abs.FunArgName)
        | HappyAbsSyn52 ([Language.Sprite.Syntax.Front.Abs.FunArgName])
        | HappyAbsSyn53 (Language.Sprite.Syntax.Front.Abs.FuncAppArg)
        | HappyAbsSyn54 ([Language.Sprite.Syntax.Front.Abs.FuncAppArg])

{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["'&&'","'\\''","'('","')'","'*'","'*/'","'+'","','","'-'","'/*@'","'/*M'","'/*Q'","':'","';'","'<'","'<='","'='","'=='","'=>'","'>'","'>='","'?'","'['","']'","'_'","'bool'","'else'","'false'","'if'","'int'","'let'","'rec'","'switch'","'true'","'type'","'val'","'{'","'|'","'||'","'}'","L_integ","L_VarIdent","L_ConIdent","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xff\xff\xff\xff\xe4\xff\xff\xff\x00\x00\x00\x00\xf7\xff\xff\xff\x3b\x00\x00\x00\x2e\x00\x00\x00\x31\x00\x00\x00\x62\x00\x00\x00\x00\x00\x00\x00\x6b\x00\x00\x00\x45\x00\x00\x00\xf5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x5f\x00\x00\x00\x30\x01\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x7a\x00\x00\x00\x84\x00\x00\x00\x90\x00\x00\x00\x93\x00\x00\x00\x79\x01\x00\x00\xcc\x00\x00\x00\x99\x00\x00\x00\x00\x00\x00\x00\x99\x01\x00\x00\x83\x00\x00\x00\x30\x01\x00\x00\xcd\x01\x00\x00\x85\x00\x00\x00\x8e\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x00\x00\x95\x00\x00\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x01\x00\x00\x0c\x00\x00\x00\xc8\x00\x00\x00\xd4\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\xa7\x00\x00\x00\x3a\x01\x00\x00\xe6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\x00\x00\x00\xef\x00\x00\x00\x1c\x01\x00\x00\xd8\x00\x00\x00\x00\x00\x00\x00\xff\x00\x00\x00\x05\x01\x00\x00\x1c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x1c\x01\x00\x00\x1c\x01\x00\x00\x25\x01\x00\x00\x0c\x01\x00\x00\x08\x01\x00\x00\x0f\x01\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x38\x01\x00\x00\x37\x01\x00\x00\x3d\x01\x00\x00\x28\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x01\x00\x00\x51\x01\x00\x00\x53\x01\x00\x00\x52\x01\x00\x00\x45\x01\x00\x00\x00\x00\x00\x00\x64\x01\x00\x00\x54\x01\x00\x00\x6f\x01\x00\x00\x7e\x01\x00\x00\x30\x01\x00\x00\x80\x01\x00\x00\x3a\x01\x00\x00\x82\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\xff\xff\x3a\x01\x00\x00\x00\x00\x00\x00\x72\x01\x00\x00\x3a\x01\x00\x00\x3a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x01\x00\x00\x94\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x87\x01\x00\x00\x9a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x01\x00\x00\xac\x01\x00\x00\xa1\x01\x00\x00\x3a\x01\x00\x00\x00\x00\x00\x00\xa5\x01\x00\x00\xbb\x01\x00\x00\x30\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x01\x00\x00\xa8\x01\x00\x00\x00\x00\x00\x00\xd3\x01\x00\x00\x34\x01\x00\x00\x00\x00\x00\x00\xd7\x01\x00\x00\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\x01\x00\x00\xdd\x01\x00\x00\xd5\x01\x00\x00\xd4\x01\x00\x00\x3b\x01\x00\x00\xe1\x01\x00\x00\x34\x01\x00\x00\x00\x00\x00\x00\xe3\x01\x00\x00\xc4\x01\x00\x00\xde\x01\x00\x00\x00\x00\x00\x00\xdf\x01\x00\x00\x30\x01\x00\x00\x30\x01\x00\x00\xe5\x01\x00\x00\x30\x01\x00\x00\x00\x00\x00\x00\xc8\x01\x00\x00\x00\x00\x00\x00\x34\x01\x00\x00\x00\x00\x00\x00\x47\x01\x00\x00\x00\x00\x00\x00\xd9\x01\x00\x00\xca\x01\x00\x00\xcb\x01\x00\x00\xc7\x01\x00\x00\xcc\x01\x00\x00\x00\x00\x00\x00\xcf\x01\x00\x00\xe7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x01\x00\x00\xe6\x01\x00\x00\xf0\xff\xff\xff\xf2\x01\x00\x00\xf4\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\xf0\x01\x00\x00\xf8\x01\x00\x00\xfc\x01\x00\x00\xec\x01\x00\x00\xd4\x01\x00\x00\x3b\x01\x00\x00\x3b\x01\x00\x00\x3b\x01\x00\x00\x3b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\x01\x00\x00\xf7\x01\x00\x00\xfd\x01\x00\x00\x00\x00\x00\x00\xeb\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x01\x00\x00\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\x01\x00\x00\xdb\x01\x00\x00\xe2\x01\x00\x00\xdc\x01\x00\x00\x00\x00\x00\x00\xf0\xff\xff\xff\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\x01\x00\x00\x30\x01\x00\x00\x05\x02\x00\x00\x30\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe9\x01\x00\x00\x40\x01\x00\x00\xf6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xe2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x00\xf2\x00\x00\x00\x0b\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x01\x00\x00\xfa\x00\x00\x00\x71\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x01\x00\x00\x07\x00\x00\x00\x0c\x02\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x00\x00\x00\xfe\x01\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\xff\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x14\x02\x00\x00\x81\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x15\x02\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x17\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x02\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x55\x01\x00\x00\x00\x00\x00\x00\xc4\x00\x00\x00\x65\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x86\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x02\x00\x00\x89\x00\x00\x00\x00\x00\x00\x00\x08\x02\x00\x00\x79\x00\x00\x00\xa1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xae\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x6b\x01\x00\x00\xf6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfd\x00\x00\x00\x00\x00\x00\x00\x02\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x54\x00\x00\x00\x00\x00\x00\x00\xad\x01\x00\x00\x00\x00\x00\x00\x04\x01\x00\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x01\x00\x00\x00\x00\x00\x00\x1b\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbb\x00\x00\x00\x04\x02\x00\x00\x97\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x01\x00\x00\xb7\x01\x00\x00\xce\x01\x00\x00\x7a\x01\x00\x00\x9d\x01\x00\x00\xa3\x01\x00\x00\xaa\x01\x00\x00\x74\x01\x00\x00\x6d\x01\x00\x00\x67\x01\x00\x00\xba\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x1d\x02\x00\x00\x00\x00\x00\x00\xf1\x00\x00\x00\x00\x00\x00\x00\xc7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf9\xff\xff\xff\x00\x00\x00\x00\xfe\xff\xff\xff\x00\x00\x00\x00\xf9\xff\xff\xff\xf3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xff\xff\xf3\xff\xff\xff\xc3\xff\xff\xff\x00\x00\x00\x00\xf8\xff\xff\xff\xf1\xff\xff\xff\xf0\xff\xff\xff\x00\x00\x00\x00\xfc\xff\xff\xff\xc3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\xff\xff\x00\x00\x00\x00\xb9\xff\xff\xff\x92\xff\xff\xff\x91\xff\xff\xff\xe3\xff\xff\xff\xfb\xff\xff\xff\x93\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\xff\xff\xff\xc2\xff\xff\xff\x00\x00\x00\x00\xc5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xce\xff\xff\xff\x9a\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9b\xff\xff\xff\x9c\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\xff\xff\xff\x95\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xff\xff\xd8\xff\xff\xff\xd7\xff\xff\xff\xd4\xff\xff\xff\xd3\xff\xff\xff\xd5\xff\xff\xff\xd2\xff\xff\xff\xd1\xff\xff\xff\xe9\xff\xff\xff\x00\x00\x00\x00\xec\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf7\xff\xff\xff\xf5\xff\xff\xff\x00\x00\x00\x00\x98\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x92\xff\xff\xff\x91\xff\xff\xff\x93\xff\xff\xff\x90\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\xff\xff\xff\xcc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\xff\xff\xce\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb8\xff\xff\xff\xc9\xff\xff\xff\xc8\xff\xff\xff\x00\x00\x00\x00\xcb\xff\xff\xff\xcd\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xca\xff\xff\xff\xb7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x94\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe2\xff\xff\xff\xeb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xba\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb2\xff\xff\xff\xb4\xff\xff\xff\x00\x00\x00\x00\xb3\xff\xff\xff\xa1\xff\xff\xff\xa0\xff\xff\xff\xae\xff\xff\xff\xac\xff\xff\xff\xaa\xff\xff\xff\xa5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x97\xff\xff\xff\xbd\xff\xff\xff\xc0\xff\xff\xff\x00\x00\x00\x00\x8f\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\xff\xff\x00\x00\x00\x00\xcf\xff\xff\xff\x00\x00\x00\x00\xc6\xff\xff\xff\x00\x00\x00\x00\xc7\xff\xff\xff\x00\x00\x00\x00\xdc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdf\xff\xff\xff\xc4\xff\xff\xff\x00\x00\x00\x00\xbb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\xff\xff\xff\x9e\xff\xff\xff\x00\x00\x00\x00\xaf\xff\xff\xff\xad\xff\xff\xff\xab\xff\xff\xff\xa6\xff\xff\xff\xa7\xff\xff\xff\xa8\xff\xff\xff\xa9\xff\xff\xff\xa3\xff\xff\xff\xa4\xff\xff\xff\xa2\xff\xff\xff\xfa\xff\xff\xff\xb0\xff\xff\xff\x00\x00\x00\x00\xb6\xff\xff\xff\x00\x00\x00\x00\xc1\xff\xff\xff\x00\x00\x00\x00\xbf\xff\xff\xff\xe0\xff\xff\xff\xe8\xff\xff\xff\x00\x00\x00\x00\xd9\xff\xff\xff\xe6\xff\xff\xff\xdb\xff\xff\xff\xd0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xff\xff\x00\x00\x00\x00\xb1\xff\xff\xff\x00\x00\x00\x00\x9d\xff\xff\xff\xb5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\xff\xff\xda\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\xff\xff\xbc\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x1a\x00\x00\x00\x17\x00\x00\x00\x0d\x00\x00\x00\x0c\x00\x00\x00\x2a\x00\x00\x00\x0c\x00\x00\x00\x04\x00\x00\x00\x0c\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x2b\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x2d\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x1f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x30\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x30\x00\x00\x00\x0d\x00\x00\x00\x0c\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x30\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x21\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x2b\x00\x00\x00\x30\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x2b\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x04\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x30\x00\x00\x00\x0e\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x30\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x01\x00\x00\x00\x2b\x00\x00\x00\x01\x00\x00\x00\x24\x00\x00\x00\x30\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x30\x00\x00\x00\x01\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x09\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x15\x00\x00\x00\x05\x00\x00\x00\x15\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x20\x00\x00\x00\x30\x00\x00\x00\x15\x00\x00\x00\x20\x00\x00\x00\x04\x00\x00\x00\x15\x00\x00\x00\x20\x00\x00\x00\x30\x00\x00\x00\x20\x00\x00\x00\x01\x00\x00\x00\x20\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x20\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x20\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x2b\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x25\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x15\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2b\x00\x00\x00\x20\x00\x00\x00\x04\x00\x00\x00\x15\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x20\x00\x00\x00\x07\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x00\x00\x2b\x00\x00\x00\x0c\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x18\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x0f\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x2a\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x14\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x2a\x00\x00\x00\x01\x00\x00\x00\x27\x00\x00\x00\x0e\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x2d\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x12\x00\x00\x00\x0c\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x09\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x0a\x00\x00\x00\x0e\x00\x00\x00\x0c\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x04\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x12\x00\x00\x00\x04\x00\x00\x00\x1d\x00\x00\x00\x2b\x00\x00\x00\x0b\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x23\x00\x00\x00\x09\x00\x00\x00\x06\x00\x00\x00\x05\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x09\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x27\x00\x00\x00\x20\x00\x00\x00\x1d\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x05\x00\x00\x00\x23\x00\x00\x00\x05\x00\x00\x00\x1f\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x1d\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x23\x00\x00\x00\x12\x00\x00\x00\x2b\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x2b\x00\x00\x00\x0a\x00\x00\x00\x2b\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x14\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0e\x00\x00\x00\x0a\x00\x00\x00\x2b\x00\x00\x00\x0c\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x05\x00\x00\x00\x0a\x00\x00\x00\x05\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x0f\x00\x00\x00\x22\x00\x00\x00\x18\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x22\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x09\x00\x00\x00\x22\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x05\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x27\x00\x00\x00\x27\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x2d\x00\x00\x00\x0a\x00\x00\x00\x0f\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x27\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x19\x00\x00\x00\x0a\x00\x00\x00\x26\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x0f\x00\x00\x00\x0c\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x22\x00\x00\x00\x27\x00\x00\x00\x0a\x00\x00\x00\x29\x00\x00\x00\x0c\x00\x00\x00\x27\x00\x00\x00\x22\x00\x00\x00\x2d\x00\x00\x00\x26\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x27\x00\x00\x00\x26\x00\x00\x00\x22\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x27\x00\x00\x00\x22\x00\x00\x00\x06\x00\x00\x00\x2c\x00\x00\x00\x08\x00\x00\x00\x03\x00\x00\x00\x0a\x00\x00\x00\x0a\x00\x00\x00\x22\x00\x00\x00\x0c\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x13\x00\x00\x00\x28\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x13\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x27\x00\x00\x00\x07\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x27\x00\x00\x00\x22\x00\x00\x00\x27\x00\x00\x00\x19\x00\x00\x00\x2c\x00\x00\x00\x29\x00\x00\x00\x29\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x29\x00\x00\x00\x09\x00\x00\x00\x14\x00\x00\x00\x07\x00\x00\x00\x2c\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x13\x00\x00\x00\x09\x00\x00\x00\x0e\x00\x00\x00\x05\x00\x00\x00\x18\x00\x00\x00\x1c\x00\x00\x00\x04\x00\x00\x00\x2b\x00\x00\x00\x2b\x00\x00\x00\x26\x00\x00\x00\x14\x00\x00\x00\x05\x00\x00\x00\x27\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x1f\x00\x00\x00\x19\x00\x00\x00\x0d\x00\x00\x00\x13\x00\x00\x00\x29\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x18\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x1d\x00\x00\x00\x12\x00\x00\x00\xff\xff\xff\xff\x1e\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x55\x00\x00\x00\x56\x00\x00\x00\x55\x00\x00\x00\x56\x00\x00\x00\x55\x00\x00\x00\x56\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x33\x00\x00\x00\x76\x00\x00\x00\x07\x00\x00\x00\x57\x00\x00\x00\x03\x00\x00\x00\x57\x00\x00\x00\x6d\x00\x00\x00\x57\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x31\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x6e\x00\x00\x00\x09\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x09\x00\x00\x00\x09\x00\x00\x00\x11\x00\x00\x00\x45\x00\x00\x00\x1e\x00\x00\x00\xff\xff\xff\xff\x1f\x00\x00\x00\x20\x00\x00\x00\x79\x00\x00\x00\x1e\x00\x00\x00\x94\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x34\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x35\x00\x00\x00\x58\x00\x00\x00\x5a\x00\x00\x00\x58\x00\x00\x00\x59\x00\x00\x00\x58\x00\x00\x00\x97\x00\x00\x00\x21\x00\x00\x00\x98\x00\x00\x00\x1e\x00\x00\x00\x0c\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x55\x00\x00\x00\x56\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x95\x00\x00\x00\x96\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x21\x00\x00\x00\x07\x00\x00\x00\x57\x00\x00\x00\xab\x00\x00\x00\x1e\x00\x00\x00\x21\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\xaa\x00\x00\x00\x1e\x00\x00\x00\x39\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x5d\x00\x00\x00\x39\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x09\x00\x00\x00\x21\x00\x00\x00\x39\x00\x00\x00\xa8\x00\x00\x00\x1e\x00\x00\x00\x09\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\xea\x00\x00\x00\x1e\x00\x00\x00\x16\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x14\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x61\x00\x00\x00\x2b\x00\x00\x00\x21\x00\x00\x00\x55\x00\x00\x00\x56\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x21\x00\x00\x00\xe8\x00\x00\x00\x1e\x00\x00\x00\x0c\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x2b\x00\x00\x00\x0f\x00\x00\x00\x57\x00\x00\x00\x2b\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x2b\x00\x00\x00\x09\x00\x00\x00\x2b\x00\x00\x00\x14\x00\x00\x00\x21\x00\x00\x00\x3a\x00\x00\x00\x7c\x00\x00\x00\x2b\x00\x00\x00\x4f\x00\x00\x00\x21\x00\x00\x00\x2b\x00\x00\x00\x3a\x00\x00\x00\xe6\x00\x00\x00\x4e\x00\x00\x00\x6f\x00\x00\x00\x70\x00\x00\x00\x71\x00\x00\x00\x6f\x00\x00\x00\x70\x00\x00\x00\xa3\x00\x00\x00\x2c\x00\x00\x00\x4d\x00\x00\x00\x65\x00\x00\x00\x4c\x00\x00\x00\xcd\x00\x00\x00\x2d\x00\x00\x00\x21\x00\x00\x00\x77\x00\x00\x00\x2d\x00\x00\x00\x49\x00\x00\x00\x73\x00\x00\x00\x2d\x00\x00\x00\x5d\x00\x00\x00\x2d\x00\x00\x00\x2b\x00\x00\x00\x47\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x38\x00\x00\x00\x2b\x00\x00\x00\x09\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x39\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x6e\x00\x00\x00\xce\x00\x00\x00\xcf\x00\x00\x00\x16\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x94\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x09\x00\x00\x00\x2d\x00\x00\x00\x36\x00\x00\x00\x9b\x00\x00\x00\x89\x00\x00\x00\x16\x00\x00\x00\x8a\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x17\x00\x00\x00\x50\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x2d\x00\x00\x00\x6b\x00\x00\x00\x4a\x00\x00\x00\x89\x00\x00\x00\x09\x00\x00\x00\x8a\x00\x00\x00\x04\x00\x00\x00\x0c\x00\x00\x00\x95\x00\x00\x00\xd2\x00\x00\x00\x69\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\xed\xff\xff\xff\x8b\x00\x00\x00\x8c\x00\x00\x00\x8d\x00\x00\x00\x8e\x00\x00\x00\x8f\x00\x00\x00\x90\x00\x00\x00\xbe\x00\x00\x00\xbf\x00\x00\x00\xc0\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x6a\x00\x00\x00\x8b\x00\x00\x00\x8c\x00\x00\x00\x8d\x00\x00\x00\x8e\x00\x00\x00\x8f\x00\x00\x00\x90\x00\x00\x00\xbe\x00\x00\x00\xbf\x00\x00\x00\xe2\x00\x00\x00\xcd\x00\x00\x00\xed\xff\xff\xff\x65\x00\x00\x00\xed\xff\xff\xff\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\xed\xff\xff\xff\x09\x00\x00\x00\x0a\x00\x00\x00\x09\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x89\x00\x00\x00\x63\x00\x00\x00\x8a\x00\x00\x00\x09\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x89\x00\x00\x00\x60\x00\x00\x00\x8a\x00\x00\x00\x5f\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x89\x00\x00\x00\x31\x00\x00\x00\x8a\x00\x00\x00\xce\x00\x00\x00\xe3\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x89\x00\x00\x00\x52\x00\x00\x00\x8a\x00\x00\x00\x8b\x00\x00\x00\x8c\x00\x00\x00\x8d\x00\x00\x00\x8e\x00\x00\x00\x8f\x00\x00\x00\x90\x00\x00\x00\x91\x00\x00\x00\x8b\x00\x00\x00\x8c\x00\x00\x00\x8d\x00\x00\x00\x8e\x00\x00\x00\x8f\x00\x00\x00\x90\x00\x00\x00\xb1\x00\x00\x00\x8b\x00\x00\x00\x8c\x00\x00\x00\x8d\x00\x00\x00\x8e\x00\x00\x00\x8f\x00\x00\x00\x90\x00\x00\x00\xa4\x00\x00\x00\x8b\x00\x00\x00\x8c\x00\x00\x00\x8d\x00\x00\x00\x8e\x00\x00\x00\x8f\x00\x00\x00\x90\x00\x00\x00\xec\x00\x00\x00\x23\x00\x00\x00\x09\x00\x00\x00\x14\x00\x00\x00\x55\x00\x00\x00\x93\x00\x00\x00\x25\x00\x00\x00\x09\x00\x00\x00\x24\x00\x00\x00\x87\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x29\x00\x00\x00\x86\x00\x00\x00\xb4\x00\x00\x00\x85\x00\x00\x00\xb5\x00\x00\x00\x93\x00\x00\x00\xb6\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x11\x00\x00\x00\x29\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x83\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x84\x00\x00\x00\x27\x00\x00\x00\x25\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x82\x00\x00\x00\x29\x00\x00\x00\x81\x00\x00\x00\x35\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x11\x00\x00\x00\x25\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x11\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x29\x00\x00\x00\x80\x00\x00\x00\x09\x00\x00\x00\x35\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x03\x00\x00\x00\x09\x00\x00\x00\x11\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x09\x00\x00\x00\x89\x00\x00\x00\x09\x00\x00\x00\x8a\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x89\x00\x00\x00\x7e\x00\x00\x00\x8a\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x7c\x00\x00\x00\x89\x00\x00\x00\x09\x00\x00\x00\x8a\x00\x00\x00\x52\x00\x00\x00\x53\x00\x00\x00\x7b\x00\x00\x00\x89\x00\x00\x00\x79\x00\x00\x00\x8a\x00\x00\x00\x77\x00\x00\x00\xef\xff\xff\xff\x8b\x00\x00\x00\x69\x00\x00\x00\xc1\x00\x00\x00\x8e\x00\x00\x00\x8f\x00\x00\x00\x90\x00\x00\x00\x8b\x00\x00\x00\x4f\x00\x00\x00\x2f\x00\x00\x00\xc2\x00\x00\x00\x8f\x00\x00\x00\x90\x00\x00\x00\xa3\x00\x00\x00\x8b\x00\x00\x00\x52\x00\x00\x00\x93\x00\x00\x00\xa2\x00\x00\x00\xc3\x00\x00\x00\x90\x00\x00\x00\x8b\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\xef\xff\xff\xff\xc7\x00\x00\x00\xef\xff\xff\xff\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\xef\xff\xff\xff\x89\x00\x00\x00\xee\xff\xff\xff\x8a\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x89\x00\x00\x00\xa1\x00\x00\x00\x8a\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\xa0\x00\x00\x00\x89\x00\x00\x00\x9f\x00\x00\x00\x8a\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\x89\x00\x00\x00\x9e\x00\x00\x00\x8a\x00\x00\x00\xa5\x00\x00\x00\xa6\x00\x00\x00\x8b\x00\x00\x00\xee\xff\xff\xff\x89\x00\x00\x00\xee\xff\xff\xff\x8a\x00\x00\x00\xc6\x00\x00\x00\x8b\x00\x00\x00\xee\xff\xff\xff\x9d\x00\x00\x00\xa5\x00\x00\x00\xd8\x00\x00\x00\xc5\x00\x00\x00\x9b\x00\x00\x00\x8b\x00\x00\x00\x9a\x00\x00\x00\x87\x00\x00\x00\x88\x00\x00\x00\x0e\x00\x00\x00\xc4\x00\x00\x00\xca\x00\x00\x00\x3e\x00\x00\x00\x11\x00\x00\x00\x3f\x00\x00\x00\x31\x00\x00\x00\x40\x00\x00\x00\x89\x00\x00\x00\xc9\x00\x00\x00\x8a\x00\x00\x00\xf1\xff\xff\xff\xbe\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\xbc\x00\x00\x00\x43\x00\x00\x00\xbd\x00\x00\x00\x44\x00\x00\x00\x45\x00\x00\x00\xb7\x00\x00\x00\xb8\x00\x00\x00\xb3\x00\x00\x00\xb1\x00\x00\x00\xbb\x00\x00\x00\xb9\x00\x00\x00\xba\x00\x00\x00\xaf\x00\x00\x00\xaa\x00\x00\x00\xae\x00\x00\x00\xad\x00\x00\x00\xa8\x00\x00\x00\xc8\x00\x00\x00\xa8\x00\x00\x00\xda\x00\x00\x00\x11\x00\x00\x00\xd8\x00\x00\x00\xd6\x00\x00\x00\xd4\x00\x00\x00\xcd\x00\x00\x00\xd5\x00\x00\x00\xe2\x00\x00\x00\xd2\x00\x00\x00\xcc\x00\x00\x00\x11\x00\x00\x00\xe1\x00\x00\x00\xbc\x00\x00\x00\xbb\x00\x00\x00\xe0\x00\x00\x00\x6e\x00\x00\x00\xdf\x00\x00\x00\xde\x00\x00\x00\xdd\x00\x00\x00\xdc\x00\x00\x00\x09\x00\x00\x00\x09\x00\x00\x00\xe6\x00\x00\x00\xe8\x00\x00\x00\xea\x00\x00\x00\xec\x00\x00\x00\x07\x00\x00\x00\x19\x00\x00\x00\x4a\x00\x00\x00\xef\x00\x00\x00\x47\x00\x00\x00\x3c\x00\x00\x00\xee\x00\x00\x00\x36\x00\x00\x00\x67\x00\x00\x00\x66\x00\x00\x00\x63\x00\x00\x00\x6b\x00\x00\x00\x60\x00\x00\x00\x5b\x00\x00\x00\x7e\x00\x00\x00\x74\x00\x00\x00\x72\x00\x00\x00\xd6\x00\x00\x00\xe4\x00\x00\x00\xaf\x00\x00\x00\xda\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 112) [
        (1 , happyReduce_1),
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57),
        (58 , happyReduce_58),
        (59 , happyReduce_59),
        (60 , happyReduce_60),
        (61 , happyReduce_61),
        (62 , happyReduce_62),
        (63 , happyReduce_63),
        (64 , happyReduce_64),
        (65 , happyReduce_65),
        (66 , happyReduce_66),
        (67 , happyReduce_67),
        (68 , happyReduce_68),
        (69 , happyReduce_69),
        (70 , happyReduce_70),
        (71 , happyReduce_71),
        (72 , happyReduce_72),
        (73 , happyReduce_73),
        (74 , happyReduce_74),
        (75 , happyReduce_75),
        (76 , happyReduce_76),
        (77 , happyReduce_77),
        (78 , happyReduce_78),
        (79 , happyReduce_79),
        (80 , happyReduce_80),
        (81 , happyReduce_81),
        (82 , happyReduce_82),
        (83 , happyReduce_83),
        (84 , happyReduce_84),
        (85 , happyReduce_85),
        (86 , happyReduce_86),
        (87 , happyReduce_87),
        (88 , happyReduce_88),
        (89 , happyReduce_89),
        (90 , happyReduce_90),
        (91 , happyReduce_91),
        (92 , happyReduce_92),
        (93 , happyReduce_93),
        (94 , happyReduce_94),
        (95 , happyReduce_95),
        (96 , happyReduce_96),
        (97 , happyReduce_97),
        (98 , happyReduce_98),
        (99 , happyReduce_99),
        (100 , happyReduce_100),
        (101 , happyReduce_101),
        (102 , happyReduce_102),
        (103 , happyReduce_103),
        (104 , happyReduce_104),
        (105 , happyReduce_105),
        (106 , happyReduce_106),
        (107 , happyReduce_107),
        (108 , happyReduce_108),
        (109 , happyReduce_109),
        (110 , happyReduce_110),
        (111 , happyReduce_111),
        (112 , happyReduce_112)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x03\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x03\x00\x00\x00\x08\x00\x00\x00\x05\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x04\x00\x00\x00\x0b\x00\x00\x00\x0b\x00\x00\x00\x0b\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x07\x00\x00\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x0b\x00\x00\x00\x07\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x06\x00\x00\x00\x0f\x00\x00\x00\x07\x00\x00\x00\x0f\x00\x00\x00\x06\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x10\x00\x00\x00\x05\x00\x00\x00\x11\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x02\x00\x00\x00\x12\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x13\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x05\x00\x00\x00\x14\x00\x00\x00\x03\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x15\x00\x00\x00\x03\x00\x00\x00\x15\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x03\x00\x00\x00\x15\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x01\x00\x00\x00\x17\x00\x00\x00\x01\x00\x00\x00\x17\x00\x00\x00\x03\x00\x00\x00\x18\x00\x00\x00\x03\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x07\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x02\x00\x00\x00\x1b\x00\x00\x00\x03\x00\x00\x00\x1c\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x03\x00\x00\x00\x1d\x00\x00\x00\x03\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x1e\x00\x00\x00\x06\x00\x00\x00\x1e\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x03\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x03\x00\x00\x00\x20\x00\x00\x00\x03\x00\x00\x00\x21\x00\x00\x00\x01\x00\x00\x00\x21\x00\x00\x00\x03\x00\x00\x00\x22\x00\x00\x00\x01\x00\x00\x00\x22\x00\x00\x00\x01\x00\x00\x00\x22\x00\x00\x00\x01\x00\x00\x00\x22\x00\x00\x00\x04\x00\x00\x00\x22\x00\x00\x00\x03\x00\x00\x00\x23\x00\x00\x00\x03\x00\x00\x00\x23\x00\x00\x00\x01\x00\x00\x00\x24\x00\x00\x00\x03\x00\x00\x00\x24\x00\x00\x00\x01\x00\x00\x00\x25\x00\x00\x00\x03\x00\x00\x00\x25\x00\x00\x00\x01\x00\x00\x00\x26\x00\x00\x00\x03\x00\x00\x00\x26\x00\x00\x00\x03\x00\x00\x00\x26\x00\x00\x00\x03\x00\x00\x00\x26\x00\x00\x00\x03\x00\x00\x00\x26\x00\x00\x00\x01\x00\x00\x00\x27\x00\x00\x00\x03\x00\x00\x00\x27\x00\x00\x00\x03\x00\x00\x00\x27\x00\x00\x00\x03\x00\x00\x00\x27\x00\x00\x00\x01\x00\x00\x00\x28\x00\x00\x00\x01\x00\x00\x00\x29\x00\x00\x00\x01\x00\x00\x00\x2a\x00\x00\x00\x01\x00\x00\x00\x2a\x00\x00\x00\x03\x00\x00\x00\x2b\x00\x00\x00\x01\x00\x00\x00\x2b\x00\x00\x00\x01\x00\x00\x00\x2b\x00\x00\x00\x01\x00\x00\x00\x2c\x00\x00\x00\x02\x00\x00\x00\x2d\x00\x00\x00\x01\x00\x00\x00\x2d\x00\x00\x00\x03\x00\x00\x00\x2e\x00\x00\x00\x01\x00\x00\x00\x2f\x00\x00\x00\x01\x00\x00\x00\x2f\x00\x00\x00\x03\x00\x00\x00\x30\x00\x00\x00\x01\x00\x00\x00\x30\x00\x00\x00\x01\x00\x00\x00\x30\x00\x00\x00\x01\x00\x00\x00\x31\x00\x00\x00\x01\x00\x00\x00\x31\x00\x00\x00\x03\x00\x00\x00"#

happyCatchStates :: [Happy_Prelude.Int]
happyCatchStates = []

happy_n_terms = 46 :: Happy_Prelude.Int
happy_n_nonterms = 50 :: Happy_Prelude.Int

happy_n_starts = 1 :: Happy_Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TI happy_var_1)))
         =  HappyAbsSyn5
                 ((read happy_var_1) :: Integer
        )
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
         =  HappyAbsSyn6
                 (Language.Sprite.Syntax.Front.Abs.VarIdent happy_var_1
        )
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (T_ConIdent happy_var_1)))
         =  HappyAbsSyn7
                 (Language.Sprite.Syntax.Front.Abs.ConIdent happy_var_1
        )
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 4# 3# happyReduction_4
happyReduction_4 ((HappyAbsSyn16  happy_var_4) `HappyStk`
        (HappyAbsSyn31  happy_var_3) `HappyStk`
        (HappyAbsSyn14  happy_var_2) `HappyStk`
        (HappyAbsSyn10  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn8
                 (Language.Sprite.Syntax.Front.Abs.Program happy_var_1 happy_var_2 happy_var_3 happy_var_4
        ) `HappyStk` happyRest

happyReduce_5 = happyReduce 10# 4# happyReduction_5
happyReduction_5 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn39  happy_var_8) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn12  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Language.Sprite.Syntax.Front.Abs.Qualifier happy_var_2 happy_var_4 happy_var_8
        ) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_0  5# happyReduction_6
happyReduction_6  =  HappyAbsSyn10
                 ([]
        )

happyReduce_7 = happySpecReduce_2  5# happyReduction_7
happyReduction_7 (HappyAbsSyn10  happy_var_2)
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn10
                 ((:) happy_var_1 happy_var_2
        )
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6# happyReduction_8
happyReduction_8 (HappyAbsSyn48  happy_var_3)
        _
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn11
                 (Language.Sprite.Syntax.Front.Abs.QualifierArg happy_var_1 happy_var_3
        )
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  7# happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn12
                 ((:[]) happy_var_1
        )
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7# happyReduction_10
happyReduction_10 (HappyAbsSyn12  happy_var_3)
        _
        (HappyAbsSyn11  happy_var_1)
         =  HappyAbsSyn12
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 5# 8# happyReduction_11
happyReduction_11 (_ `HappyStk`
        (HappyAbsSyn26  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn15  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn13
                 (Language.Sprite.Syntax.Front.Abs.Measure happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_0  9# happyReduction_12
happyReduction_12  =  HappyAbsSyn14
                 ([]
        )

happyReduce_13 = happySpecReduce_2  9# happyReduction_13
happyReduction_13 (HappyAbsSyn14  happy_var_2)
        (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn14
                 ((:) happy_var_1 happy_var_2
        )
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10# happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn15
                 (Language.Sprite.Syntax.Front.Abs.MeasureIdAsVar happy_var_1
        )
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10# happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn15
                 (Language.Sprite.Syntax.Front.Abs.MeasureIdAsCon happy_var_1
        )
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11# happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.ConstInt happy_var_1
        )
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11# happyReduction_17
happyReduction_17 (HappyAbsSyn17  happy_var_1)
         =  HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.Bool happy_var_1
        )
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  11# happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.Var happy_var_1
        )
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  11# happyReduction_19
happyReduction_19 (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.ConApp happy_var_1 happy_var_2
        )
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4# 11# happyReduction_20
happyReduction_20 (_ `HappyStk`
        (HappyAbsSyn54  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.FunApp happy_var_1 happy_var_3
        ) `HappyStk` happyRest

happyReduce_21 = happyReduce 11# 11# happyReduction_21
happyReduction_21 (_ `HappyStk`
        (HappyAbsSyn16  happy_var_10) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn16  happy_var_6) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn53  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.If happy_var_3 happy_var_6 happy_var_10
        ) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_2  11# happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_2)
        (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.Let happy_var_1 happy_var_2
        )
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 7# 11# happyReduction_23
happyReduction_23 (_ `HappyStk`
        (HappyAbsSyn16  happy_var_6) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn52  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.Fun happy_var_2 happy_var_6
        ) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  11# happyReduction_24
happyReduction_24 (HappyAbsSyn53  happy_var_3)
        (HappyAbsSyn24  happy_var_2)
        (HappyAbsSyn53  happy_var_1)
         =  HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.Op happy_var_1 happy_var_2 happy_var_3
        )
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 7# 11# happyReduction_25
happyReduction_25 (_ `HappyStk`
        (HappyAbsSyn22  happy_var_6) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 (Language.Sprite.Syntax.Front.Abs.Switch happy_var_3 happy_var_6
        ) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  12# happyReduction_26
happyReduction_26 _
         =  HappyAbsSyn17
                 (Language.Sprite.Syntax.Front.Abs.ConstTrue
        )

happyReduce_27 = happySpecReduce_1  12# happyReduction_27
happyReduction_27 _
         =  HappyAbsSyn17
                 (Language.Sprite.Syntax.Front.Abs.ConstFalse
        )

happyReduce_28 = happySpecReduce_0  13# happyReduction_28
happyReduction_28  =  HappyAbsSyn18
                 (Language.Sprite.Syntax.Front.Abs.EmptyConAppArgs
        )

happyReduce_29 = happySpecReduce_3  13# happyReduction_29
happyReduction_29 _
        (HappyAbsSyn54  happy_var_2)
        _
         =  HappyAbsSyn18
                 (Language.Sprite.Syntax.Front.Abs.NonEmptyConAppArgs happy_var_2
        )
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 6# 14# happyReduction_30
happyReduction_30 (_ `HappyStk`
        (HappyAbsSyn26  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn19
                 (Language.Sprite.Syntax.Front.Abs.Annotation happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyReduce_31 = happyReduce 7# 15# happyReduction_31
happyReduction_31 (_ `HappyStk`
        (HappyAbsSyn16  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_4) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn20
                 (Language.Sprite.Syntax.Front.Abs.RecDecl happy_var_1 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

happyReduce_32 = happyReduce 6# 15# happyReduction_32
happyReduction_32 (_ `HappyStk`
        (HappyAbsSyn16  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn20
                 (Language.Sprite.Syntax.Front.Abs.AnnotatedDecl happy_var_1 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyReduce_33 = happyReduce 5# 15# happyReduction_33
happyReduction_33 (_ `HappyStk`
        (HappyAbsSyn16  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn20
                 (Language.Sprite.Syntax.Front.Abs.UnAnnotatedDecl happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyReduce_34 = happyReduce 5# 16# happyReduction_34
happyReduction_34 ((HappyAbsSyn16  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn23  happy_var_3) `HappyStk`
        (HappyAbsSyn7  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn21
                 (Language.Sprite.Syntax.Front.Abs.SwitchCase happy_var_2 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_1  17# happyReduction_35
happyReduction_35 (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn22
                 ((:[]) happy_var_1
        )
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  17# happyReduction_36
happyReduction_36 (HappyAbsSyn22  happy_var_2)
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn22
                 ((:) happy_var_1 happy_var_2
        )
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18# happyReduction_37
happyReduction_37 _
        (HappyAbsSyn52  happy_var_2)
        _
         =  HappyAbsSyn23
                 (Language.Sprite.Syntax.Front.Abs.SwitchCaseNonEmptyDataConArgs happy_var_2
        )
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  18# happyReduction_38
happyReduction_38  =  HappyAbsSyn23
                 (Language.Sprite.Syntax.Front.Abs.SwitchCaseEmptyDataConArgs
        )

happyReduce_39 = happySpecReduce_1  19# happyReduction_39
happyReduction_39 _
         =  HappyAbsSyn24
                 (Language.Sprite.Syntax.Front.Abs.IntPlus
        )

happyReduce_40 = happySpecReduce_1  19# happyReduction_40
happyReduction_40 _
         =  HappyAbsSyn24
                 (Language.Sprite.Syntax.Front.Abs.IntMinus
        )

happyReduce_41 = happySpecReduce_1  19# happyReduction_41
happyReduction_41 _
         =  HappyAbsSyn24
                 (Language.Sprite.Syntax.Front.Abs.IntMultiply
        )

happyReduce_42 = happySpecReduce_1  19# happyReduction_42
happyReduction_42 _
         =  HappyAbsSyn24
                 (Language.Sprite.Syntax.Front.Abs.IntEq
        )

happyReduce_43 = happySpecReduce_1  19# happyReduction_43
happyReduction_43 _
         =  HappyAbsSyn24
                 (Language.Sprite.Syntax.Front.Abs.IntLessThan
        )

happyReduce_44 = happySpecReduce_1  19# happyReduction_44
happyReduction_44 _
         =  HappyAbsSyn24
                 (Language.Sprite.Syntax.Front.Abs.IntLessOrEqThan
        )

happyReduce_45 = happySpecReduce_1  19# happyReduction_45
happyReduction_45 _
         =  HappyAbsSyn24
                 (Language.Sprite.Syntax.Front.Abs.IntGreaterThan
        )

happyReduce_46 = happySpecReduce_1  19# happyReduction_46
happyReduction_46 _
         =  HappyAbsSyn24
                 (Language.Sprite.Syntax.Front.Abs.IntGreaterOrEqThan
        )

happyReduce_47 = happyReduce 5# 20# happyReduction_47
happyReduction_47 (_ `HappyStk`
        (HappyAbsSyn39  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn25
                 (Language.Sprite.Syntax.Front.Abs.KnownRefinement happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_3  20# happyReduction_48
happyReduction_48 _
        _
        _
         =  HappyAbsSyn25
                 (Language.Sprite.Syntax.Front.Abs.UnknownRefinement
        )

happyReduce_49 = happySpecReduce_0  20# happyReduction_49
happyReduction_49  =  HappyAbsSyn25
                 (Language.Sprite.Syntax.Front.Abs.SimpleRefinement
        )

happyReduce_50 = happySpecReduce_3  21# happyReduction_50
happyReduction_50 (HappyAbsSyn26  happy_var_3)
        _
        (HappyAbsSyn37  happy_var_1)
         =  HappyAbsSyn26
                 (Language.Sprite.Syntax.Front.Abs.TypeFun happy_var_1 happy_var_3
        )
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  21# happyReduction_51
happyReduction_51 (HappyAbsSyn25  happy_var_2)
        (HappyAbsSyn48  happy_var_1)
         =  HappyAbsSyn26
                 (Language.Sprite.Syntax.Front.Abs.TypeRefined happy_var_1 happy_var_2
        )
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  21# happyReduction_52
happyReduction_52 (HappyAbsSyn25  happy_var_3)
        (HappyAbsSyn29  happy_var_2)
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn26
                 (Language.Sprite.Syntax.Front.Abs.TypeData happy_var_1 happy_var_2 happy_var_3
        )
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  21# happyReduction_53
happyReduction_53 _
        (HappyAbsSyn26  happy_var_2)
        _
         =  HappyAbsSyn26
                 (happy_var_2
        )
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  22# happyReduction_54
happyReduction_54 (HappyAbsSyn26  happy_var_1)
         =  HappyAbsSyn27
                 (Language.Sprite.Syntax.Front.Abs.TypeDataArg happy_var_1
        )
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  23# happyReduction_55
happyReduction_55 (HappyAbsSyn27  happy_var_1)
         =  HappyAbsSyn28
                 ((:[]) happy_var_1
        )
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  23# happyReduction_56
happyReduction_56 (HappyAbsSyn28  happy_var_3)
        _
        (HappyAbsSyn27  happy_var_1)
         =  HappyAbsSyn28
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  24# happyReduction_57
happyReduction_57 _
        (HappyAbsSyn28  happy_var_2)
        _
         =  HappyAbsSyn29
                 (Language.Sprite.Syntax.Front.Abs.NonEmptyTypeDataArgs happy_var_2
        )
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_0  24# happyReduction_58
happyReduction_58  =  HappyAbsSyn29
                 (Language.Sprite.Syntax.Front.Abs.EmptyTypeDataArgs
        )

happyReduce_59 = happyReduce 7# 25# happyReduction_59
happyReduction_59 (_ `HappyStk`
        (HappyAbsSyn33  happy_var_6) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn36  happy_var_3) `HappyStk`
        (HappyAbsSyn6  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn30
                 (Language.Sprite.Syntax.Front.Abs.DataType happy_var_2 happy_var_3 happy_var_6
        ) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_0  26# happyReduction_60
happyReduction_60  =  HappyAbsSyn31
                 ([]
        )

happyReduce_61 = happySpecReduce_2  26# happyReduction_61
happyReduction_61 (HappyAbsSyn31  happy_var_2)
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn31
                 ((:) happy_var_1 happy_var_2
        )
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  27# happyReduction_62
happyReduction_62 (HappyAbsSyn35  happy_var_3)
        (HappyAbsSyn34  happy_var_2)
        (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn32
                 (Language.Sprite.Syntax.Front.Abs.DataTypeConstructor happy_var_1 happy_var_2 happy_var_3
        )
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  28# happyReduction_63
happyReduction_63 (HappyAbsSyn32  happy_var_1)
         =  HappyAbsSyn33
                 ((:[]) happy_var_1
        )
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  28# happyReduction_64
happyReduction_64 (HappyAbsSyn33  happy_var_3)
        _
        (HappyAbsSyn32  happy_var_1)
         =  HappyAbsSyn33
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  29# happyReduction_65
happyReduction_65 _
        (HappyAbsSyn38  happy_var_2)
        _
         =  HappyAbsSyn34
                 (Language.Sprite.Syntax.Front.Abs.NonEmptyDataTypeConstructorArgs happy_var_2
        )
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_0  29# happyReduction_66
happyReduction_66  =  HappyAbsSyn34
                 (Language.Sprite.Syntax.Front.Abs.EmptyDataTypeConstructorArgs
        )

happyReduce_67 = happyReduce 6# 30# happyReduction_67
happyReduction_67 (_ `HappyStk`
        (HappyAbsSyn39  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn35
                 (Language.Sprite.Syntax.Front.Abs.NonEmptyDataTypeConstructorPredicate happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyReduce_68 = happySpecReduce_0  30# happyReduction_68
happyReduction_68  =  HappyAbsSyn35
                 (Language.Sprite.Syntax.Front.Abs.EmptyDataTypeConstructorPredicate
        )

happyReduce_69 = happySpecReduce_3  31# happyReduction_69
happyReduction_69 _
        (HappyAbsSyn50  happy_var_2)
        _
         =  HappyAbsSyn36
                 (Language.Sprite.Syntax.Front.Abs.NonEmptyDataTypeArgs happy_var_2
        )
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  31# happyReduction_70
happyReduction_70  =  HappyAbsSyn36
                 (Language.Sprite.Syntax.Front.Abs.EmptyDataTypeArgs
        )

happyReduce_71 = happySpecReduce_3  32# happyReduction_71
happyReduction_71 (HappyAbsSyn26  happy_var_3)
        _
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn37
                 (Language.Sprite.Syntax.Front.Abs.NamedFuncArg happy_var_1 happy_var_3
        )
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  32# happyReduction_72
happyReduction_72 (HappyAbsSyn26  happy_var_3)
        _
        _
         =  HappyAbsSyn37
                 (Language.Sprite.Syntax.Front.Abs.UnNamedFuncArg happy_var_3
        )
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  33# happyReduction_73
happyReduction_73 (HappyAbsSyn37  happy_var_1)
         =  HappyAbsSyn38
                 ((:[]) happy_var_1
        )
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  33# happyReduction_74
happyReduction_74 (HappyAbsSyn38  happy_var_3)
        _
        (HappyAbsSyn37  happy_var_1)
         =  HappyAbsSyn38
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  34# happyReduction_75
happyReduction_75 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PVar happy_var_1
        )
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  34# happyReduction_76
happyReduction_76 (HappyAbsSyn17  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PBool happy_var_1
        )
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  34# happyReduction_77
happyReduction_77 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PInt happy_var_1
        )
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happyReduce 4# 34# happyReduction_78
happyReduction_78 (_ `HappyStk`
        (HappyAbsSyn47  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn15  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PMeasure happy_var_1 happy_var_3
        ) `HappyStk` happyRest

happyReduce_79 = happySpecReduce_3  34# happyReduction_79
happyReduction_79 _
        (HappyAbsSyn39  happy_var_2)
        _
         =  HappyAbsSyn39
                 (happy_var_2
        )
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  35# happyReduction_80
happyReduction_80 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.POr happy_var_1 happy_var_3
        )
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  35# happyReduction_81
happyReduction_81 (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (happy_var_1
        )
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  36# happyReduction_82
happyReduction_82 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PAnd happy_var_1 happy_var_3
        )
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  36# happyReduction_83
happyReduction_83 (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (happy_var_1
        )
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  37# happyReduction_84
happyReduction_84 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PEq happy_var_1 happy_var_3
        )
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  37# happyReduction_85
happyReduction_85 (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (happy_var_1
        )
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  38# happyReduction_86
happyReduction_86 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PLessThan happy_var_1 happy_var_3
        )
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_3  38# happyReduction_87
happyReduction_87 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PLessOrEqThan happy_var_1 happy_var_3
        )
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  38# happyReduction_88
happyReduction_88 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PGreaterThan happy_var_1 happy_var_3
        )
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  38# happyReduction_89
happyReduction_89 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PGreaterOrEqThan happy_var_1 happy_var_3
        )
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  38# happyReduction_90
happyReduction_90 (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (happy_var_1
        )
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  39# happyReduction_91
happyReduction_91 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PPlus happy_var_1 happy_var_3
        )
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  39# happyReduction_92
happyReduction_92 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PMinus happy_var_1 happy_var_3
        )
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  39# happyReduction_93
happyReduction_93 (HappyAbsSyn39  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (Language.Sprite.Syntax.Front.Abs.PMultiply happy_var_1 happy_var_3
        )
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  39# happyReduction_94
happyReduction_94 (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (happy_var_1
        )
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  40# happyReduction_95
happyReduction_95 (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (happy_var_1
        )
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  41# happyReduction_96
happyReduction_96 (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn46
                 (Language.Sprite.Syntax.Front.Abs.MeasureArg happy_var_1
        )
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  42# happyReduction_97
happyReduction_97 (HappyAbsSyn46  happy_var_1)
         =  HappyAbsSyn47
                 ((:[]) happy_var_1
        )
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_3  42# happyReduction_98
happyReduction_98 (HappyAbsSyn47  happy_var_3)
        _
        (HappyAbsSyn46  happy_var_1)
         =  HappyAbsSyn47
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  43# happyReduction_99
happyReduction_99 _
         =  HappyAbsSyn48
                 (Language.Sprite.Syntax.Front.Abs.BaseTypeInt
        )

happyReduce_100 = happySpecReduce_1  43# happyReduction_100
happyReduction_100 _
         =  HappyAbsSyn48
                 (Language.Sprite.Syntax.Front.Abs.BaseTypeBool
        )

happyReduce_101 = happySpecReduce_1  43# happyReduction_101
happyReduction_101 (HappyAbsSyn49  happy_var_1)
         =  HappyAbsSyn48
                 (Language.Sprite.Syntax.Front.Abs.BaseTypeVar happy_var_1
        )
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_2  44# happyReduction_102
happyReduction_102 (HappyAbsSyn6  happy_var_2)
        _
         =  HappyAbsSyn49
                 (Language.Sprite.Syntax.Front.Abs.TypeVarId happy_var_2
        )
happyReduction_102 _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  45# happyReduction_103
happyReduction_103 (HappyAbsSyn49  happy_var_1)
         =  HappyAbsSyn50
                 ((:[]) happy_var_1
        )
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_3  45# happyReduction_104
happyReduction_104 (HappyAbsSyn50  happy_var_3)
        _
        (HappyAbsSyn49  happy_var_1)
         =  HappyAbsSyn50
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_104 _ _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_1  46# happyReduction_105
happyReduction_105 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn51
                 (Language.Sprite.Syntax.Front.Abs.FunArgName happy_var_1
        )
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  47# happyReduction_106
happyReduction_106 (HappyAbsSyn51  happy_var_1)
         =  HappyAbsSyn52
                 ((:[]) happy_var_1
        )
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  47# happyReduction_107
happyReduction_107 (HappyAbsSyn52  happy_var_3)
        _
        (HappyAbsSyn51  happy_var_1)
         =  HappyAbsSyn52
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  48# happyReduction_108
happyReduction_108 (HappyAbsSyn17  happy_var_1)
         =  HappyAbsSyn53
                 (Language.Sprite.Syntax.Front.Abs.FuncAppArgBool happy_var_1
        )
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  48# happyReduction_109
happyReduction_109 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn53
                 (Language.Sprite.Syntax.Front.Abs.FuncAppArgInt happy_var_1
        )
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  48# happyReduction_110
happyReduction_110 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn53
                 (Language.Sprite.Syntax.Front.Abs.FuncAppArgVar happy_var_1
        )
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  49# happyReduction_111
happyReduction_111 (HappyAbsSyn53  happy_var_1)
         =  HappyAbsSyn54
                 ((:[]) happy_var_1
        )
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  49# happyReduction_112
happyReduction_112 (HappyAbsSyn54  happy_var_3)
        _
        (HappyAbsSyn53  happy_var_1)
         =  HappyAbsSyn54
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_112 _ _ _  = notHappyAtAll 

happyTerminalToTok term = case term of {
        PT _ (TS _ 1) -> 2#;
        PT _ (TS _ 2) -> 3#;
        PT _ (TS _ 3) -> 4#;
        PT _ (TS _ 4) -> 5#;
        PT _ (TS _ 5) -> 6#;
        PT _ (TS _ 6) -> 7#;
        PT _ (TS _ 7) -> 8#;
        PT _ (TS _ 8) -> 9#;
        PT _ (TS _ 9) -> 10#;
        PT _ (TS _ 10) -> 11#;
        PT _ (TS _ 11) -> 12#;
        PT _ (TS _ 12) -> 13#;
        PT _ (TS _ 13) -> 14#;
        PT _ (TS _ 14) -> 15#;
        PT _ (TS _ 15) -> 16#;
        PT _ (TS _ 16) -> 17#;
        PT _ (TS _ 17) -> 18#;
        PT _ (TS _ 18) -> 19#;
        PT _ (TS _ 19) -> 20#;
        PT _ (TS _ 20) -> 21#;
        PT _ (TS _ 21) -> 22#;
        PT _ (TS _ 22) -> 23#;
        PT _ (TS _ 23) -> 24#;
        PT _ (TS _ 24) -> 25#;
        PT _ (TS _ 25) -> 26#;
        PT _ (TS _ 26) -> 27#;
        PT _ (TS _ 27) -> 28#;
        PT _ (TS _ 28) -> 29#;
        PT _ (TS _ 29) -> 30#;
        PT _ (TS _ 30) -> 31#;
        PT _ (TS _ 31) -> 32#;
        PT _ (TS _ 32) -> 33#;
        PT _ (TS _ 33) -> 34#;
        PT _ (TS _ 34) -> 35#;
        PT _ (TS _ 35) -> 36#;
        PT _ (TS _ 36) -> 37#;
        PT _ (TS _ 37) -> 38#;
        PT _ (TS _ 38) -> 39#;
        PT _ (TS _ 39) -> 40#;
        PT _ (TS _ 40) -> 41#;
        PT _ (TI happy_dollar_dollar) -> 42#;
        PT _ (T_VarIdent happy_dollar_dollar) -> 43#;
        PT _ (T_ConIdent happy_dollar_dollar) -> 44#;
        _ -> -1#;
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend  _kmore []       = kend notHappyAtAll []
happyLex _kend kmore  (tk:tks) = kmore (happyTerminalToTok tk) tk tks
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 45# notHappyAtAll action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 45# tk explist resume tks = happyReport' tks explist resume
happyReport _ tk explist resume tks = happyReport' (tk:tks) explist (\tks -> resume (Happy_Prelude.tail tks))


happyThen :: () => (Err a) -> (a -> (Err b)) -> (Err b)
happyThen = ((>>=))
happyReturn :: () => a -> (Err a)
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyFmap1 f m tks = happyThen (m tks) (\a -> happyReturn (f a))
happyReturn1 :: () => a -> b -> (Err a)
happyReturn1 = \a tks -> (return) a
happyReport' :: () => [(Token)] -> [Happy_Prelude.String] -> ([(Token)] -> (Err a)) -> (Err a)
happyReport' = (\tokens expected resume -> happyError tokens)

happyAbort :: () => [(Token)] -> (Err a)
happyAbort = Happy_Prelude.error "Called abort handler in non-resumptive parser"

pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Happy_Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Happy_Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Happy_Prelude.Bool)
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define INVALID_TOK -1#
#define ERROR_TOK 0#
#define CATCH_TOK 1#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) Happy_Prelude.$
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO Happy_Prelude.$ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    Happy_Prelude.return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++
              ",\ttoken: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# rule) Happy_Prelude.++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Happy_Prelude.Just (Happy_GHC_Exts.I# act) -> act
  Happy_Prelude.Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(i, 0#), GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  -- i >= 0:   Guard against INVALID_TOK (do the default action, which ultimately errors)
  -- off >= 0: Otherwise it's a default action
  -- equality check: Ensure that the entry in the compressed array is owned by st
  = Happy_Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | Happy_Prelude.otherwise
  = Happy_Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state
  deriving Happy_Prelude.Show

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | Happy_Prelude.otherwise = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

happyIndexRuleArr :: Happy_Int -> (# Happy_Int, Happy_Int #)
happyIndexRuleArr r = (# nt, len #)
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),2#)
    nt = happyIndexOffAddr happyRuleArr offs
    len = happyIndexOffAddr happyRuleArr PLUS(offs,1#)

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     -- See "Error Fixup" below
     let i = GET_ERROR_TOKEN(x) in
     DEBUG_TRACE("shifting the error token")
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st sts stk
     = happySeq fn (happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_2 nt fn j tk old_st
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_3 nt fn j tk old_st
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                st `happyTcHack` happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          j `happyTcHack` happyThen1 (fn stk tk)
                                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyIndexOffAddr happyGotoOffsets st1
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            j `happyTcHack` happyThen1 (fn stk tk)
                                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

{- Note [Error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~
When there is no applicable action for the current lookahead token `tk`,
happy enters error recovery mode. Depending on whether the grammar file
declares the two action form `%error { abort } { report }` for
    Resumptive Error Handling,
it works in one (not resumptive) or two phases (resumptive):

 1. Fixup mode:
    Try to see if there is an action for the error token ERROR_TOK. If there
    is, do *not* emit an error and pretend instead that an `error` token was
    inserted.
    When there is no ERROR_TOK action, report an error.

    In non-resumptive error handling, calling the single error handler
    (e.g. `happyError`) will throw an exception and abort the parser.
    However, in resumptive error handling we enter *error resumption mode*.

 2. Error resumption mode:
    After reporting the error (with `report`), happy will attempt to find
    a good state stack to resume parsing in.
    For each candidate stack, it discards input until one of the candidates
    resumes (i.e. shifts the current input).
    If no candidate resumes before the end of input, resumption failed and
    calls the `abort` function, to much the same effect as in non-resumptive
    error handling.

    Candidate stacks are declared by the grammar author using the special
    `catch` terminal and called "catch frames".
    This mechanism is described in detail in Note [happyResume].

The `catch` resumption mechanism (2) is what usually is associated with
`error` in `bison` or `menhir`. Since `error` is used for the Fixup mechanism
(1) above, we call the corresponding token `catch`.
Furthermore, in constrast to `bison`, our implementation of `catch`
non-deterministically considers multiple catch frames on the stack for
resumption (See Note [Multiple catch frames]).

Note [happyResume]
~~~~~~~~~~~~~~~~~~
`happyResume` implements the resumption mechanism from Note [Error recovery].
It is best understood by example. Consider

Exp :: { String }
Exp : '1'                { "1" }
    | catch              { "catch" }
    | Exp '+' Exp %shift { $1 Happy_Prelude.++ " + " Happy_Prelude.++ $3 } -- %shift: associate 1 + 1 + 1 to the right
    | '(' Exp ')'        { "(" Happy_Prelude.++ $2 Happy_Prelude.++ ")" }

The idea of the use of `catch` here is that upon encountering a parse error
during expression parsing, we can gracefully degrade using the `catch` rule,
still producing a partial syntax tree and keep on parsing to find further
syntax errors.

Let's trace the parser state for input 11+1, which will error out after shifting 1.
After shifting, we have the following item stack (growing downwards and omitting
transitive closure items):

  State 0: %start_parseExp -> . Exp
  State 5: Exp -> '1' .

(Stack as a list of state numbers: [5,0].)
As Note [Error recovery] describes, we will first try Fixup mode.
That fails because no production can shift the `error` token.
Next we try Error resumption mode. This works as follows:

  1. Pop off the item stack until we find an item that can shift the `catch`
     token. (Implemented in `pop_items`.)
       * State 5 cannot shift catch. Pop.
       * State 0 can shift catch, which would transition into
          State 4: Exp -> catch .
     So record the *stack* `[4,0]` after doing the shift transition.
     We call this a *catch frame*, where the top is a *catch state*,
     corresponding to an item in which we just shifted a `catch` token.
     There can be multiple such catch stacks, see Note [Multiple catch frames].

  2. Discard tokens from the input until the lookahead can be shifted in one
     of the catch stacks. (Implemented in `discard_input_until_exp` and
     `some_catch_state_shifts`.)
       * We cannot shift the current lookahead '1' in state 4, so we discard
       * We *can* shift the next lookahead '+' in state 4, but only after
         reducing, which pops State 4 and goes to State 3:
           State 3: %start_parseExp -> Exp .
                    Exp -> Exp . '+' Exp
         Here we can shift '+'.
     As you can see, to implement this machinery we need to simulate
     the operation of the LALR automaton, especially reduction
     (`happySimulateReduce`).

Note [Multiple catch frames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For fewer spurious error messages, it can be beneficial to trace multiple catch
items. Consider

Exp : '1'
    | catch
    | Exp '+' Exp %shift
    | '(' Exp ')'

Let's trace the parser state for input (;+1, which will error out after shifting (.
After shifting, we have the following item stack (growing downwards):

  State 0: %start_parseExp -> . Exp
  State 6: Exp -> '(' . Exp ')'

Upon error, we want to find items in the stack which can shift a catch token.
Note that both State 0 and State 6 can shift a catch token, transitioning into
  State 4: Exp -> catch .
Hence we record the catch frames `[4,6,0]` and `[4,0]` for possible resumption.

Which catch frame do we pick for resumption?
Note that resuming catch frame `[4,0]` will parse as "catch+1", whereas
resuming the innermost frame `[4,6,0]` corresponds to parsing "(catch+1".
The latter would keep discarding input until the closing ')' is found.
So we will discard + and 1, leading to a spurious syntax error at the end of
input, aborting the parse and never producing a partial syntax tree. Bad!

It is far preferable to resume with catch frame `[4,0]`, where we can resume
successfully on input +, so that is what we do.

In general, we pick the catch frame for resumption that discards the least
amount of input for a successful shift, preferring the topmost such catch frame.
-}

-- happyFail :: Happy_Int -> Token -> Happy_Int -> _
-- This function triggers Note [Error recovery].
-- If the current token is ERROR_TOK, phase (1) has failed and we might try
-- phase (2).
happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

-- Enter Error Fixup (see Note [Error recovery]):
-- generate an error token, save the old token and carry on.
-- When a `happyShift` accepts the error token, we will pop off the error token
-- to resume parsing with the current lookahead `i`.
happyTryFixup i tk action sts stk =
  DEBUG_TRACE("entering `error` fixup.\n")
  happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- See Note [Error recovery], phase (2).
-- Enter resumption mode after reporting the error by calling `happyResume`.
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = GET_ERROR_TOKEN(x) in
  DEBUG_TRACE("`error` fixup failed.\n")
  let resume   = happyResume i tk st sts stk
      expected = happyExpectedTokens st sts in
  happyReport i tk expected resume

-- happyResume :: Happy_Int -> Token -> Happy_Int -> _
-- See Note [happyResume]
happyResume i tk st sts stk = pop_items [] st sts stk
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts   -- this is to test whether we have a start token
    !(Happy_GHC_Exts.I# eof_i) = happy_n_terms Happy_Prelude.- 1   -- this is the token number of the EOF token
    happy_list_to_list :: Happy_IntList -> [Happy_Prelude.Int]
    happy_list_to_list (HappyCons st sts)
      | LT(st, n_starts)
      = [(Happy_GHC_Exts.I# st)]
      | Happy_Prelude.otherwise
      = (Happy_GHC_Exts.I# st) : happy_list_to_list sts

    -- See (1) of Note [happyResume]
    pop_items catch_frames st sts stk
      | LT(st, n_starts)
      = DEBUG_TRACE("reached start state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", ")
        if Happy_Prelude.null catch_frames_new
          then DEBUG_TRACE("no resumption.\n")
               happyAbort
          else DEBUG_TRACE("now discard input, trying to anchor in states " Happy_Prelude.++ Happy_Prelude.show (Happy_Prelude.map (happy_list_to_list . Happy_Prelude.fst) (Happy_Prelude.reverse catch_frames_new)) Happy_Prelude.++ ".\n")
               discard_input_until_exp i tk (Happy_Prelude.reverse catch_frames_new)
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = pop_items catch_frames_new st1 sts1 stk1
      where
        !catch_frames_new
          | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
          , DEBUG_TRACE("can shift catch token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", into state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
            Happy_Prelude.null (Happy_Prelude.filter (\(HappyCons _ (HappyCons h _),_) -> EQ(st,h)) catch_frames)
          = (HappyCons new_state (HappyCons st sts), MK_ERROR_TOKEN(i) `HappyStk` stk):catch_frames -- MK_ERROR_TOKEN(i) is just some dummy that should not be accessed by user code
          | Happy_Prelude.otherwise
          = DEBUG_TRACE("already shifted or can't shift catch in " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ "\n")
            catch_frames

    -- See (2) of Note [happyResume]
    discard_input_until_exp i tk catch_frames
      | Happy_Prelude.Just (HappyCons st (HappyCons catch_st sts), catch_frame) <- some_catch_state_shifts i catch_frames
      = DEBUG_TRACE("found expected token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ " after shifting from " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# catch_st) Happy_Prelude.++ ": " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyDoAction i tk st (HappyCons catch_st sts) catch_frame
      | EQ(i,eof_i) -- is i EOF?
      = DEBUG_TRACE("reached EOF, cannot resume. abort parse :(\n")
        happyAbort
      | Happy_Prelude.otherwise
      = DEBUG_TRACE("discard token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyLex (\eof_tk -> discard_input_until_exp eof_i eof_tk catch_frames) -- eof
                 (\i tk   -> discard_input_until_exp i tk catch_frames)         -- not eof

    some_catch_state_shifts _ [] = DEBUG_TRACE("no catch state could shift.\n") Happy_Prelude.Nothing
    some_catch_state_shifts i catch_frames@(((HappyCons st sts),_):_) = try_head i st sts catch_frames
      where
        try_head i st sts catch_frames = -- PRECONDITION: head catch_frames = (HappyCons st sts)
          DEBUG_TRACE("trying token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ " in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ": ")
          case happyDecodeAction (happyNextAction i st) of
            HappyFail     -> DEBUG_TRACE("fail.\n")   some_catch_state_shifts i (Happy_Prelude.tail catch_frames)
            HappyAccept   -> DEBUG_TRACE("accept.\n") Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyShift _  -> DEBUG_TRACE("shift.\n")  Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyReduce r -> case happySimulateReduce r st sts of
              (HappyCons st1 sts1) -> try_head i st1 sts1 catch_frames

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# r) Happy_Prelude.++ ", ")
  let (# nt, len #) = happyIndexRuleArr r in
  DEBUG_TRACE("nt " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# nt) Happy_Prelude.++ ", len: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# len) Happy_Prelude.++ ", new_st ")
  let !(sts1@(HappyCons st1 _)) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(Happy_Prelude.show (Happy_GHC_Exts.I# new_st) Happy_Prelude.++ ".\n")
  (HappyCons new_st sts1)

happyTokenToString :: Happy_Prelude.Int -> Happy_Prelude.String
happyTokenToString i = happyTokenStrings Happy_Prelude.!! (i Happy_Prelude.- 2) -- 2: errorTok, catchTok

happyExpectedTokens :: Happy_Int -> Happy_IntList -> [Happy_Prelude.String]
-- Upon a parse error, we want to suggest tokens that are expected in that
-- situation. This function computes such tokens.
-- It works by examining the top of the state stack.
-- For every token number that does a shift transition, record that token number.
-- For every token number that does a reduce transition, simulate that reduction
-- on the state state stack and repeat.
-- The recorded token numbers are then formatted with 'happyTokenToString' and
-- returned.
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  Happy_Prelude.map happyTokenToString (search_shifts st sts [])
  where
    search_shifts st sts shifts = Happy_Prelude.foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      DEBUG_TRACE("found action in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", input " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ ", " Happy_Prelude.++ Happy_Prelude.show (happyDecodeAction act) Happy_Prelude.++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Prelude.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          (HappyCons st1 sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      -- The (token number, action) pairs of all actions in the given state
      = ((-1), (Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st)))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i) -- happyIndexActionTable with cached row offset
      | let off_i = PLUS(off,i)
      , GTE(off_i,0#)
      , EQ(happyIndexOffAddr happyCheck off_i,i)
      = [(Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i))]
      | Happy_Prelude.otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Happy_Prelude.error "Internal Happy parser panic. This is not supposed to happen! Please open a bug report at https://github.com/haskell/happy/issues.\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Happy_GHC_Exts.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
