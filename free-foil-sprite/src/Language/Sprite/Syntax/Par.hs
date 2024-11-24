{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sprite.Syntax.Par
  ( happyError
  , myLexer
  , pListDecl
  ) where

import Prelude

import qualified Language.Sprite.Syntax.Abs
import Language.Sprite.Syntax.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

data HappyAbsSyn 
        = HappyTerminal (Token)
        | HappyErrorToken Prelude.Int
        | HappyAbsSyn4 (Integer)
        | HappyAbsSyn5 (Language.Sprite.Syntax.Abs.VarIdent)
        | HappyAbsSyn6 (Language.Sprite.Syntax.Abs.Term)
        | HappyAbsSyn7 (Language.Sprite.Syntax.Abs.Ann)
        | HappyAbsSyn8 (Language.Sprite.Syntax.Abs.PlainDecl)
        | HappyAbsSyn9 (Language.Sprite.Syntax.Abs.Decl)
        | HappyAbsSyn10 ([Language.Sprite.Syntax.Abs.Decl])
        | HappyAbsSyn11 (Language.Sprite.Syntax.Abs.IntOp)
        | HappyAbsSyn12 (Language.Sprite.Syntax.Abs.RType)
        | HappyAbsSyn13 (Language.Sprite.Syntax.Abs.ScopedRType)
        | HappyAbsSyn14 (Language.Sprite.Syntax.Abs.Pred)
        | HappyAbsSyn15 (Language.Sprite.Syntax.Abs.Pattern)
        | HappyAbsSyn16 (Language.Sprite.Syntax.Abs.ScopedTerm)
        | HappyAbsSyn17 (Language.Sprite.Syntax.Abs.BaseType)
        | HappyAbsSyn18 (Language.Sprite.Syntax.Abs.ArgList)
        | HappyAbsSyn19 ([Language.Sprite.Syntax.Abs.ArgList])

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x04\x40\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x20\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x10\x00\x80\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x04\x00\x00\x00\x00\x01\x00\x08\x06\x00\x80\x00\x00\x04\x03\x00\x00\x00\x00\x01\x01\x00\x00\x20\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\xc1\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x08\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x20\x18\x00\x00\x00\x00\x24\x0c\x00\x00\x00\x00\x04\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x62\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x84\x01\x00\x00\x00\x40\xc2\x00\x00\x00\x00\x20\x61\x00\x00\x00\x00\x90\x30\x00\x00\x00\x00\x48\x18\x00\x00\x00\x00\x24\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pListDecl","Integer","VarIdent","Term","Ann","PlainDecl","Decl","ListDecl","IntOp","RType","ScopedRType","Pred","Pattern","ScopedTerm","BaseType","ArgList","ListArgList","'('","')'","'*'","'*/'","'+'","','","'-'","'/*@'","':'","';'","'<'","'<='","'='","'=='","'=>'","'['","']'","'false'","'int'","'let'","'true'","'val'","'{'","'|'","'}'","L_integ","L_VarIdent","%eof"]
        bit_start = st               Prelude.* 47
        bit_end   = (st Prelude.+ 1) Prelude.* 47
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..46]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x11\x00\x00\x00\xee\xff\xff\xff\x00\x00\x00\x00\xf9\xff\xff\xff\x00\x00\x00\x00\x11\x00\x00\x00\xf2\xff\xff\xff\x10\x00\x00\x00\x19\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x37\x00\x00\x00\x4f\x00\x00\x00\x44\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x54\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x00\x00\x00\x64\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\x5a\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x67\x00\x00\x00\x5f\x00\x00\x00\x61\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x42\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x65\x00\x00\x00\x00\x00\x00\x00\x42\x00\x00\x00\x42\x00\x00\x00\x42\x00\x00\x00\x42\x00\x00\x00\x42\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x2e\x00\x00\x00\x2e\x00\x00\x00\x2e\x00\x00\x00\x2e\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x6a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x16\x00\x00\x00\x63\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x76\x00\x00\x00\x00\x00\x00\x00\x68\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x2d\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x3f\x00\x00\x00\x41\x00\x00\x00\x43\x00\x00\x00\x45\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf1\xff\xff\xff\x00\x00\x00\x00\xfe\xff\xff\xff\x00\x00\x00\x00\xf2\xff\xff\xff\xf1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\xff\xff\x00\x00\x00\x00\xf0\xff\xff\xff\xf3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xff\xff\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xed\xff\xff\xff\xde\xff\xff\xff\xfb\xff\xff\xff\x00\x00\x00\x00\xdf\xff\xff\xff\xfa\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\xff\xff\xf4\xff\xff\xff\xdd\xff\xff\xff\xdc\xff\xff\xff\x00\x00\x00\x00\xf7\xff\xff\xff\xf6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\xff\xff\xff\x00\x00\x00\x00\xdb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\xff\xff\xff\xec\xff\xff\xff\xe7\xff\xff\xff\xea\xff\xff\xff\x00\x00\x00\x00\xe8\xff\xff\xff\xe9\xff\xff\xff\x00\x00\x00\x00\xf9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xee\xff\xff\xff\xe6\xff\xff\xff\xe4\xff\xff\xff\xe5\xff\xff\xff\xe2\xff\xff\xff\xe3\xff\xff\xff\xe1\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x03\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x1a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x0e\x00\x00\x00\x14\x00\x00\x00\x1c\x00\x00\x00\x11\x00\x00\x00\x0a\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x14\x00\x00\x00\x16\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0c\x00\x00\x00\x08\x00\x00\x00\x08\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x0d\x00\x00\x00\x0d\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x1b\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x0a\x00\x00\x00\x0d\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x1b\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x00\x00\x13\x00\x00\x00\x0a\x00\x00\x00\x10\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x09\x00\x00\x00\x12\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x0a\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x1b\x00\x00\x00\x0f\x00\x00\x00\x18\x00\x00\x00\x17\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x01\x00\x00\x00\x07\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x3e\x00\x00\x00\x15\x00\x00\x00\x3f\x00\x00\x00\x22\x00\x00\x00\x40\x00\x00\x00\x22\x00\x00\x00\x20\x00\x00\x00\x03\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x21\x00\x00\x00\x43\x00\x00\x00\x09\x00\x00\x00\xff\xff\xff\xff\x44\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x23\x00\x00\x00\x30\x00\x00\x00\x09\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x1c\x00\x00\x00\x08\x00\x00\x00\x13\x00\x00\x00\x03\x00\x00\x00\x0b\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x1c\x00\x00\x00\x16\x00\x00\x00\x13\x00\x00\x00\x1d\x00\x00\x00\x16\x00\x00\x00\x16\x00\x00\x00\x09\x00\x00\x00\x0c\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x3b\x00\x00\x00\x17\x00\x00\x00\x2b\x00\x00\x00\x18\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x18\x00\x00\x00\x18\x00\x00\x00\x3e\x00\x00\x00\x20\x00\x00\x00\x3f\x00\x00\x00\x0b\x00\x00\x00\x40\x00\x00\x00\x21\x00\x00\x00\x38\x00\x00\x00\x10\x00\x00\x00\x41\x00\x00\x00\x42\x00\x00\x00\x0b\x00\x00\x00\x43\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x49\x00\x00\x00\x2a\x00\x00\x00\x48\x00\x00\x00\x1a\x00\x00\x00\x47\x00\x00\x00\x29\x00\x00\x00\x46\x00\x00\x00\x28\x00\x00\x00\x45\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x0b\x00\x00\x00\x16\x00\x00\x00\x3a\x00\x00\x00\x20\x00\x00\x00\x27\x00\x00\x00\x3b\x00\x00\x00\x2b\x00\x00\x00\x21\x00\x00\x00\x44\x00\x00\x00\x0b\x00\x00\x00\x03\x00\x00\x00\x0b\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x30\x00\x00\x00\x13\x00\x00\x00\x10\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x2f\x00\x00\x00\x13\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x25\x00\x00\x00\x2e\x00\x00\x00\x13\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x0c\x00\x00\x00\x0b\x00\x00\x00\x34\x00\x00\x00\x33\x00\x00\x00\x32\x00\x00\x00\x0d\x00\x00\x00\x09\x00\x00\x00\x0b\x00\x00\x00\x0e\x00\x00\x00\x1e\x00\x00\x00\x3d\x00\x00\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 36) [
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
        (36 , happyReduce_36)
        ]

happy_n_terms = 29 :: Prelude.Int
happy_n_nonterms = 16 :: Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TI happy_var_1)))
         =  HappyAbsSyn4
                 ((read happy_var_1) :: Integer
        )
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
         =  HappyAbsSyn5
                 (Language.Sprite.Syntax.Abs.VarIdent happy_var_1
        )
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_1)
         =  HappyAbsSyn6
                 (Language.Sprite.Syntax.Abs.ConstInt happy_var_1
        )
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  2# happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn6
                 (Language.Sprite.Syntax.Abs.Var happy_var_1
        )
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  2# happyReduction_5
happyReduction_5 (HappyAbsSyn16  happy_var_2)
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn6
                 (Language.Sprite.Syntax.Abs.Let happy_var_1 happy_var_2
        )
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 7# 2# happyReduction_6
happyReduction_6 (_ `HappyStk`
        (HappyAbsSyn16  happy_var_6) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn6
                 (Language.Sprite.Syntax.Abs.Fun happy_var_2 happy_var_6
        ) `HappyStk` happyRest

happyReduce_7 = happyReduce 4# 2# happyReduction_7
happyReduction_7 (_ `HappyStk`
        (HappyAbsSyn19  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn6
                 (Language.Sprite.Syntax.Abs.App happy_var_1 happy_var_3
        ) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  2# happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
        (HappyAbsSyn11  happy_var_2)
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (Language.Sprite.Syntax.Abs.Op happy_var_1 happy_var_2 happy_var_3
        )
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  2# happyReduction_9
happyReduction_9 _
        (HappyAbsSyn6  happy_var_2)
        _
         =  HappyAbsSyn6
                 (happy_var_2
        )
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 6# 3# happyReduction_10
happyReduction_10 (_ `HappyStk`
        (HappyAbsSyn12  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn7
                 (Language.Sprite.Syntax.Abs.Ann happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyReduce_11 = happyReduce 5# 4# happyReduction_11
happyReduction_11 (_ `HappyStk`
        (HappyAbsSyn6  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn8
                 (Language.Sprite.Syntax.Abs.PlainDecl happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_2  5# happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_2)
        (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn9
                 (Language.Sprite.Syntax.Abs.AnnotaedDecl happy_var_1 happy_var_2
        )
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  5# happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn9
                 (Language.Sprite.Syntax.Abs.UnannotaedDecl happy_var_1
        )
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  6# happyReduction_14
happyReduction_14  =  HappyAbsSyn10
                 ([]
        )

happyReduce_15 = happySpecReduce_2  6# happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_2)
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn10
                 ((:) happy_var_1 happy_var_2
        )
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  7# happyReduction_16
happyReduction_16 _
         =  HappyAbsSyn11
                 (Language.Sprite.Syntax.Abs.IntPlus
        )

happyReduce_17 = happyReduce 6# 8# happyReduction_17
happyReduction_17 (_ `HappyStk`
        (HappyAbsSyn14  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn17  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (Language.Sprite.Syntax.Abs.TypeRefined happy_var_1 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_1  8# happyReduction_18
happyReduction_18 (HappyAbsSyn17  happy_var_1)
         =  HappyAbsSyn12
                 (Language.Sprite.Syntax.Abs.TypeRefinedBase happy_var_1
        )
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happyReduce 5# 8# happyReduction_19
happyReduction_19 ((HappyAbsSyn13  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn12  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn12
                 (Language.Sprite.Syntax.Abs.TypeFun happy_var_1 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  9# happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_1)
         =  HappyAbsSyn13
                 (Language.Sprite.Syntax.Abs.ScopedRType happy_var_1
        )
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  10# happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PVar happy_var_1
        )
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  10# happyReduction_22
happyReduction_22 _
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PTrue
        )

happyReduce_23 = happySpecReduce_1  10# happyReduction_23
happyReduction_23 _
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PFalse
        )

happyReduce_24 = happySpecReduce_1  10# happyReduction_24
happyReduction_24 (HappyAbsSyn4  happy_var_1)
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PInt happy_var_1
        )
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  10# happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_3)
        _
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PEq happy_var_1 happy_var_3
        )
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  10# happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_3)
        _
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PLessThan happy_var_1 happy_var_3
        )
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  10# happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_3)
        _
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PLessOrEqThan happy_var_1 happy_var_3
        )
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  10# happyReduction_28
happyReduction_28 (HappyAbsSyn14  happy_var_3)
        _
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PPlus happy_var_1 happy_var_3
        )
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  10# happyReduction_29
happyReduction_29 (HappyAbsSyn14  happy_var_3)
        _
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PMinus happy_var_1 happy_var_3
        )
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  10# happyReduction_30
happyReduction_30 (HappyAbsSyn14  happy_var_3)
        _
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn14
                 (Language.Sprite.Syntax.Abs.PMultiply happy_var_1 happy_var_3
        )
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  11# happyReduction_31
happyReduction_31 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn15
                 (Language.Sprite.Syntax.Abs.PatternVar happy_var_1
        )
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  12# happyReduction_32
happyReduction_32 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn16
                 (Language.Sprite.Syntax.Abs.ScopedTerm happy_var_1
        )
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  13# happyReduction_33
happyReduction_33 _
         =  HappyAbsSyn17
                 (Language.Sprite.Syntax.Abs.BaseTypeInt
        )

happyReduce_34 = happySpecReduce_1  14# happyReduction_34
happyReduction_34 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn18
                 (Language.Sprite.Syntax.Abs.ArgList happy_var_1
        )
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  15# happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn19
                 ((:[]) happy_var_1
        )
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  15# happyReduction_36
happyReduction_36 (HappyAbsSyn19  happy_var_3)
        _
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn19
                 ((:) happy_var_1 happy_var_3
        )
happyReduction_36 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
        happyDoAction 28# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
        let cont i = happyDoAction i tk action sts stk tks in
        case tk of {
        PT _ (TS _ 1) -> cont 1#;
        PT _ (TS _ 2) -> cont 2#;
        PT _ (TS _ 3) -> cont 3#;
        PT _ (TS _ 4) -> cont 4#;
        PT _ (TS _ 5) -> cont 5#;
        PT _ (TS _ 6) -> cont 6#;
        PT _ (TS _ 7) -> cont 7#;
        PT _ (TS _ 8) -> cont 8#;
        PT _ (TS _ 9) -> cont 9#;
        PT _ (TS _ 10) -> cont 10#;
        PT _ (TS _ 11) -> cont 11#;
        PT _ (TS _ 12) -> cont 12#;
        PT _ (TS _ 13) -> cont 13#;
        PT _ (TS _ 14) -> cont 14#;
        PT _ (TS _ 15) -> cont 15#;
        PT _ (TS _ 16) -> cont 16#;
        PT _ (TS _ 17) -> cont 17#;
        PT _ (TS _ 18) -> cont 18#;
        PT _ (TS _ 19) -> cont 19#;
        PT _ (TS _ 20) -> cont 20#;
        PT _ (TS _ 21) -> cont 21#;
        PT _ (TS _ 22) -> cont 22#;
        PT _ (TS _ 23) -> cont 23#;
        PT _ (TS _ 24) -> cont 24#;
        PT _ (TS _ 25) -> cont 25#;
        PT _ (TI happy_dollar_dollar) -> cont 26#;
        PT _ (T_VarIdent happy_dollar_dollar) -> cont 27#;
        _ -> happyError' ((tk:tks), [])
        }

happyError_ explist 28# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pListDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

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
#if __GLASGOW_HASKELL__ > 706
#  define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#  define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#  define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#  define LT(n,m) (n Happy_GHC_Exts.<# m)
#  define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#  define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define ERROR_TOK 0#

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
#  define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
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
  DEBUG_TRACE("state: " ++ show (Happy_GHC_Exts.I# st) ++
              ",\ttoken: " ++ show (Happy_GHC_Exts.I# i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail (happyExpListPerState (Happy_GHC_Exts.I# st)) i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " ++ show (Happy_GHC_Exts.I# rule) ++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (Happy_GHC_Exts.I# act) -> act
  Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  = Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | otherwise         = HappyShift MINUS(action, 1#)

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

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (happyIndexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#))) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
-- trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st sts stk
     = happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk)
                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyAdjustOffset (happyIndexOffAddr happyGotoOffsets st1)
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            happyThen1 (fn stk tk)
                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist ERROR_TOK tk old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--      trace "failing" $
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st (HappyCons action sts)
                               (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction ERROR_TOK tk action sts (saved_tok`HappyStk`stk)
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk action sts stk =
-- trace "entering error recovery" $
        happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

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
happyDoSeq   a b = a `Prelude.seq` b
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
