-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Lambda.Syntax.Par
  ( happyError
  , myLexer
  , pTerm
  , pPattern
  , pScopedTerm
  , pBaseType
  , pKind
  , pEnvironment
  , pVarBinding
  , pListVarBinding
  ) where

import Prelude

import qualified Language.Lambda.Syntax.Abs
import Language.Lambda.Syntax.Lex

}

%name pTerm Term
%name pPattern Pattern
%name pScopedTerm ScopedTerm
%name pBaseType BaseType
%name pKind Kind
%name pEnvironment Environment
%name pVarBinding VarBinding
%name pListVarBinding ListVarBinding
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('        { PT _ (TS _ 1)        }
  ')'        { PT _ (TS _ 2)        }
  '*'        { PT _ (TS _ 3)        }
  ':'        { PT _ (TS _ 4)        }
  ';'        { PT _ (TS _ 5)        }
  '<'        { PT _ (TS _ 6)        }
  '='        { PT _ (TS _ 7)        }
  '=>'       { PT _ (TS _ 8)        }
  'B'        { PT _ (TS _ 9)        }
  '['        { PT _ (TS _ 10)       }
  ']'        { PT _ (TS _ 11)       }
  'false'    { PT _ (TS _ 12)       }
  'int'      { PT _ (TS _ 13)       }
  'let'      { PT _ (TS _ 14)       }
  'true'     { PT _ (TS _ 15)       }
  '{'        { PT _ (TS _ 16)       }
  '|'        { PT _ (TS _ 17)       }
  '}'        { PT _ (TS _ 18)       }
  '∅'        { PT _ (TS _ 19)       }
  L_integ    { PT _ (TI $$)         }
  L_VarIdent { PT _ (T_VarIdent $$) }

%%

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

VarIdent :: { Language.Lambda.Syntax.Abs.VarIdent }
VarIdent  : L_VarIdent { Language.Lambda.Syntax.Abs.VarIdent $1 }

Term :: { Language.Lambda.Syntax.Abs.Term }
Term
  : Integer { Language.Lambda.Syntax.Abs.ConstInt $1 }
  | VarIdent { Language.Lambda.Syntax.Abs.Var $1 }
  | 'let' Term '=' Pattern ';' ScopedTerm { Language.Lambda.Syntax.Abs.Let $2 $4 $6 }
  | '(' Pattern ')' '=>' '{' ScopedTerm '}' { Language.Lambda.Syntax.Abs.Fun $2 $6 }
  | Term Term { Language.Lambda.Syntax.Abs.App $1 $2 }
  | Term ':' Term { Language.Lambda.Syntax.Abs.Ann $1 $3 }
  | BaseType '[' Pattern '|' ScopedTerm ']' { Language.Lambda.Syntax.Abs.TypeRefined $1 $3 $5 }
  | Term ':' Pattern '=>' ScopedTerm { Language.Lambda.Syntax.Abs.TypeFun $1 $3 $5 }
  | 'true' { Language.Lambda.Syntax.Abs.PTrue }
  | 'false' { Language.Lambda.Syntax.Abs.PFalse }
  | Term '<' Term { Language.Lambda.Syntax.Abs.PLessThan $1 $3 }

Pattern :: { Language.Lambda.Syntax.Abs.Pattern }
Pattern : VarIdent { Language.Lambda.Syntax.Abs.PatternVar $1 }

ScopedTerm :: { Language.Lambda.Syntax.Abs.ScopedTerm }
ScopedTerm : Term { Language.Lambda.Syntax.Abs.ScopedTerm $1 }

BaseType :: { Language.Lambda.Syntax.Abs.BaseType }
BaseType : 'int' { Language.Lambda.Syntax.Abs.BaseTypeInt }

Kind :: { Language.Lambda.Syntax.Abs.Kind }
Kind
  : 'B' { Language.Lambda.Syntax.Abs.KindBase }
  | '*' { Language.Lambda.Syntax.Abs.KindStar }

Environment :: { Language.Lambda.Syntax.Abs.Environment }
Environment
  : '∅' { Language.Lambda.Syntax.Abs.EnvironmentEmpty }
  | ListVarBinding { Language.Lambda.Syntax.Abs.EnvironmentNonEmpty $1 }

VarBinding :: { Language.Lambda.Syntax.Abs.VarBinding }
VarBinding
  : VarIdent ':' Term { Language.Lambda.Syntax.Abs.VarBinding $1 $3 }

ListVarBinding :: { [Language.Lambda.Syntax.Abs.VarBinding] }
ListVarBinding
  : VarBinding { (:[]) $1 }
  | VarBinding ';' ListVarBinding { (:) $1 $3 }

{

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

}
