-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
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

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '&&'       { PT _ (TS _ 1)        }
  '\''       { PT _ (TS _ 2)        }
  '('        { PT _ (TS _ 3)        }
  ')'        { PT _ (TS _ 4)        }
  '*'        { PT _ (TS _ 5)        }
  '*/'       { PT _ (TS _ 6)        }
  '+'        { PT _ (TS _ 7)        }
  ','        { PT _ (TS _ 8)        }
  '-'        { PT _ (TS _ 9)        }
  '/**@'     { PT _ (TS _ 10)       }
  '/*@'      { PT _ (TS _ 11)       }
  ':'        { PT _ (TS _ 12)       }
  ';'        { PT _ (TS _ 13)       }
  '<'        { PT _ (TS _ 14)       }
  '<='       { PT _ (TS _ 15)       }
  '='        { PT _ (TS _ 16)       }
  '=='       { PT _ (TS _ 17)       }
  '=>'       { PT _ (TS _ 18)       }
  '>'        { PT _ (TS _ 19)       }
  '>='       { PT _ (TS _ 20)       }
  '?'        { PT _ (TS _ 21)       }
  '['        { PT _ (TS _ 22)       }
  ']'        { PT _ (TS _ 23)       }
  '_'        { PT _ (TS _ 24)       }
  'bool'     { PT _ (TS _ 25)       }
  'else'     { PT _ (TS _ 26)       }
  'false'    { PT _ (TS _ 27)       }
  'if'       { PT _ (TS _ 28)       }
  'int'      { PT _ (TS _ 29)       }
  'let'      { PT _ (TS _ 30)       }
  'qualif'   { PT _ (TS _ 31)       }
  'rec'      { PT _ (TS _ 32)       }
  'true'     { PT _ (TS _ 33)       }
  'val'      { PT _ (TS _ 34)       }
  '{'        { PT _ (TS _ 35)       }
  '|'        { PT _ (TS _ 36)       }
  '||'       { PT _ (TS _ 37)       }
  '}'        { PT _ (TS _ 38)       }
  L_integ    { PT _ (TI $$)         }
  L_VarIdent { PT _ (T_VarIdent $$) }

%%

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

VarIdent :: { Language.Sprite.Syntax.Front.Abs.VarIdent }
VarIdent  : L_VarIdent { Language.Sprite.Syntax.Front.Abs.VarIdent $1 }

Program :: { Language.Sprite.Syntax.Front.Abs.Program }
Program
  : ListQualifier Term { Language.Sprite.Syntax.Front.Abs.Program $1 $2 }

Qualifier :: { Language.Sprite.Syntax.Front.Abs.Qualifier }
Qualifier
  : '/**@' 'qualif' VarIdent '(' ListQualifierArg ')' ':' '(' Pred ')' '*/' { Language.Sprite.Syntax.Front.Abs.Qualifier $3 $5 $9 }

ListQualifier :: { [Language.Sprite.Syntax.Front.Abs.Qualifier] }
ListQualifier
  : {- empty -} { [] } | Qualifier ListQualifier { (:) $1 $2 }

QualifierArg :: { Language.Sprite.Syntax.Front.Abs.QualifierArg }
QualifierArg
  : VarIdent ':' BaseType { Language.Sprite.Syntax.Front.Abs.QualifierArg $1 $3 }

ListQualifierArg :: { [Language.Sprite.Syntax.Front.Abs.QualifierArg] }
ListQualifierArg
  : QualifierArg { (:[]) $1 }
  | QualifierArg ',' ListQualifierArg { (:) $1 $3 }

Term :: { Language.Sprite.Syntax.Front.Abs.Term }
Term
  : Integer { Language.Sprite.Syntax.Front.Abs.ConstInt $1 }
  | ConstBool { Language.Sprite.Syntax.Front.Abs.Bool $1 }
  | VarIdent { Language.Sprite.Syntax.Front.Abs.Var $1 }
  | 'if' '(' FuncAppArg ')' '{' Term '}' 'else' '{' Term '}' { Language.Sprite.Syntax.Front.Abs.If $3 $6 $10 }
  | Decl Term { Language.Sprite.Syntax.Front.Abs.Let $1 $2 }
  | '(' VarIdent ')' '=>' '{' Term '}' { Language.Sprite.Syntax.Front.Abs.Fun $2 $6 }
  | VarIdent '(' FuncAppArg ')' { Language.Sprite.Syntax.Front.Abs.App $1 $3 }
  | FuncAppArg IntOp FuncAppArg { Language.Sprite.Syntax.Front.Abs.Op $1 $2 $3 }
  | '(' Term ')' { $2 }

ConstBool :: { Language.Sprite.Syntax.Front.Abs.ConstBool }
ConstBool
  : 'true' { Language.Sprite.Syntax.Front.Abs.ConstTrue }
  | 'false' { Language.Sprite.Syntax.Front.Abs.ConstFalse }

Annotation :: { Language.Sprite.Syntax.Front.Abs.Annotation }
Annotation
  : '/*@' 'val' VarIdent ':' RType '*/' { Language.Sprite.Syntax.Front.Abs.Annotation $3 $5 }

Decl :: { Language.Sprite.Syntax.Front.Abs.Decl }
Decl
  : Annotation 'let' 'rec' VarIdent '=' Term ';' { Language.Sprite.Syntax.Front.Abs.RecDecl $1 $4 $6 }
  | Annotation 'let' VarIdent '=' Term ';' { Language.Sprite.Syntax.Front.Abs.AnnotatedDecl $1 $3 $5 }
  | 'let' VarIdent '=' Term ';' { Language.Sprite.Syntax.Front.Abs.UnAnnotatedDecl $2 $4 }

IntOp :: { Language.Sprite.Syntax.Front.Abs.IntOp }
IntOp
  : '+' { Language.Sprite.Syntax.Front.Abs.IntPlus }
  | '-' { Language.Sprite.Syntax.Front.Abs.IntMinus }
  | '*' { Language.Sprite.Syntax.Front.Abs.IntMultiply }
  | '==' { Language.Sprite.Syntax.Front.Abs.IntEq }
  | '<' { Language.Sprite.Syntax.Front.Abs.IntLessThan }
  | '<=' { Language.Sprite.Syntax.Front.Abs.IntLessOrEqThan }
  | '>' { Language.Sprite.Syntax.Front.Abs.IntGreaterThan }
  | '>=' { Language.Sprite.Syntax.Front.Abs.IntGreaterOrEqThan }

RType2 :: { Language.Sprite.Syntax.Front.Abs.RType }
RType2
  : BaseType '[' VarIdent '|' Pred ']' { Language.Sprite.Syntax.Front.Abs.TypeRefined $1 $3 $5 }
  | BaseType '[' '?' ']' { Language.Sprite.Syntax.Front.Abs.TypeRefinedUnknown $1 }
  | '\'' VarIdent { Language.Sprite.Syntax.Front.Abs.TypeVar $2 }
  | RType3 { $1 }

RType3 :: { Language.Sprite.Syntax.Front.Abs.RType }
RType3
  : BaseType { Language.Sprite.Syntax.Front.Abs.TypeRefinedSimple $1 }
  | '(' RType ')' { $2 }

RType1 :: { Language.Sprite.Syntax.Front.Abs.RType }
RType1
  : FuncArg '=>' RType2 { Language.Sprite.Syntax.Front.Abs.TypeFun $1 $3 }
  | RType2 { $1 }

RType :: { Language.Sprite.Syntax.Front.Abs.RType }
RType : RType1 { $1 }

FuncArg :: { Language.Sprite.Syntax.Front.Abs.FuncArg }
FuncArg
  : VarIdent ':' RType { Language.Sprite.Syntax.Front.Abs.NamedFuncArg $1 $3 }
  | '_' ':' RType { Language.Sprite.Syntax.Front.Abs.UnNamedFuncArg $3 }

Pred6 :: { Language.Sprite.Syntax.Front.Abs.Pred }
Pred6
  : VarIdent { Language.Sprite.Syntax.Front.Abs.PVar $1 }
  | ConstBool { Language.Sprite.Syntax.Front.Abs.PBool $1 }
  | Integer { Language.Sprite.Syntax.Front.Abs.PInt $1 }
  | '(' Pred ')' { $2 }

Pred1 :: { Language.Sprite.Syntax.Front.Abs.Pred }
Pred1
  : Pred1 '||' Pred2 { Language.Sprite.Syntax.Front.Abs.POr $1 $3 }
  | Pred2 { $1 }

Pred2 :: { Language.Sprite.Syntax.Front.Abs.Pred }
Pred2
  : Pred2 '&&' Pred3 { Language.Sprite.Syntax.Front.Abs.PAnd $1 $3 }
  | Pred3 { $1 }

Pred3 :: { Language.Sprite.Syntax.Front.Abs.Pred }
Pred3
  : Pred3 '==' Pred4 { Language.Sprite.Syntax.Front.Abs.PEq $1 $3 }
  | Pred4 { $1 }

Pred4 :: { Language.Sprite.Syntax.Front.Abs.Pred }
Pred4
  : Pred4 '<' Pred5 { Language.Sprite.Syntax.Front.Abs.PLessThan $1 $3 }
  | Pred4 '<=' Pred5 { Language.Sprite.Syntax.Front.Abs.PLessOrEqThan $1 $3 }
  | Pred4 '>' Pred5 { Language.Sprite.Syntax.Front.Abs.PGreaterThan $1 $3 }
  | Pred4 '>=' Pred5 { Language.Sprite.Syntax.Front.Abs.PGreaterOrEqThan $1 $3 }
  | Pred5 { $1 }

Pred5 :: { Language.Sprite.Syntax.Front.Abs.Pred }
Pred5
  : Pred5 '+' Pred6 { Language.Sprite.Syntax.Front.Abs.PPlus $1 $3 }
  | Pred5 '-' Pred6 { Language.Sprite.Syntax.Front.Abs.PMinus $1 $3 }
  | Pred5 '*' Pred6 { Language.Sprite.Syntax.Front.Abs.PMultiply $1 $3 }
  | Pred6 { $1 }

Pred :: { Language.Sprite.Syntax.Front.Abs.Pred }
Pred : Pred1 { $1 }

BaseType :: { Language.Sprite.Syntax.Front.Abs.BaseType }
BaseType
  : 'int' { Language.Sprite.Syntax.Front.Abs.BaseTypeInt }
  | 'bool' { Language.Sprite.Syntax.Front.Abs.BaseTypeBool }

FuncAppArg :: { Language.Sprite.Syntax.Front.Abs.FuncAppArg }
FuncAppArg
  : ConstBool { Language.Sprite.Syntax.Front.Abs.FuncAppArgBool $1 }
  | Integer { Language.Sprite.Syntax.Front.Abs.FuncAppArgInt $1 }
  | VarIdent { Language.Sprite.Syntax.Front.Abs.FuncAppArgVar $1 }

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

