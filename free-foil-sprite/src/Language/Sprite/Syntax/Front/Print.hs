-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Language.

module Language.Sprite.Syntax.Front.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Language.Sprite.Syntax.Front.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Language.Sprite.Syntax.Front.Abs.VarIdent where
  prt _ (Language.Sprite.Syntax.Front.Abs.VarIdent i) = doc $ showString i
instance Print Language.Sprite.Syntax.Front.Abs.ConIdent where
  prt _ (Language.Sprite.Syntax.Front.Abs.ConIdent i) = doc $ showString i
instance Print Language.Sprite.Syntax.Front.Abs.Program where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.Program qualifiers measures datatypes term -> prPrec i 0 (concatD [prt 0 qualifiers, prt 0 measures, prt 0 datatypes, prt 0 term])

instance Print Language.Sprite.Syntax.Front.Abs.Qualifier where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.Qualifier varident qualifierargs pred -> prPrec i 0 (concatD [doc (showString "/*Q"), prt 0 varident, doc (showString "("), prt 0 qualifierargs, doc (showString ")"), doc (showString ":"), doc (showString "("), prt 0 pred, doc (showString ")"), doc (showString "*/")])

instance Print [Language.Sprite.Syntax.Front.Abs.Qualifier] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "\n"), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.QualifierArg where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.QualifierArg varident basetype -> prPrec i 0 (concatD [prt 0 varident, doc (showString ":"), prt 0 basetype])

instance Print [Language.Sprite.Syntax.Front.Abs.QualifierArg] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.Measure where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.Measure measureident rtype -> prPrec i 0 (concatD [doc (showString "/*M"), prt 0 measureident, doc (showString ":"), prt 0 rtype, doc (showString "*/")])

instance Print [Language.Sprite.Syntax.Front.Abs.Measure] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "\n"), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.MeasureIdent where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.MeasureIdAsVar varident -> prPrec i 0 (concatD [prt 0 varident])
    Language.Sprite.Syntax.Front.Abs.MeasureIdAsCon conident -> prPrec i 0 (concatD [prt 0 conident])

instance Print Language.Sprite.Syntax.Front.Abs.Term where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.ConstInt n -> prPrec i 0 (concatD [prt 0 n])
    Language.Sprite.Syntax.Front.Abs.Bool constbool -> prPrec i 0 (concatD [prt 0 constbool])
    Language.Sprite.Syntax.Front.Abs.Var varident -> prPrec i 0 (concatD [prt 0 varident])
    Language.Sprite.Syntax.Front.Abs.ConApp conident conappargs -> prPrec i 0 (concatD [prt 0 conident, prt 0 conappargs])
    Language.Sprite.Syntax.Front.Abs.FunApp varident funcappargs -> prPrec i 0 (concatD [prt 0 varident, doc (showString "("), prt 0 funcappargs, doc (showString ")")])
    Language.Sprite.Syntax.Front.Abs.If funcapparg term1 term2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 funcapparg, doc (showString ")"), doc (showString "{"), prt 0 term1, doc (showString "}"), doc (showString "else"), doc (showString "{"), prt 0 term2, doc (showString "}")])
    Language.Sprite.Syntax.Front.Abs.Let decl term -> prPrec i 0 (concatD [prt 0 decl, prt 0 term])
    Language.Sprite.Syntax.Front.Abs.Fun funargnames term -> prPrec i 0 (concatD [doc (showString "("), prt 0 funargnames, doc (showString ")"), doc (showString "=>"), doc (showString "{"), prt 0 term, doc (showString "}")])
    Language.Sprite.Syntax.Front.Abs.Op funcapparg1 intop funcapparg2 -> prPrec i 0 (concatD [prt 0 funcapparg1, prt 0 intop, prt 0 funcapparg2])
    Language.Sprite.Syntax.Front.Abs.Switch varident switchcases -> prPrec i 0 (concatD [doc (showString "switch"), doc (showString "("), prt 0 varident, doc (showString ")"), doc (showString "{"), prt 0 switchcases, doc (showString "}")])

instance Print Language.Sprite.Syntax.Front.Abs.ConstBool where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.ConstTrue -> prPrec i 0 (concatD [doc (showString "true")])
    Language.Sprite.Syntax.Front.Abs.ConstFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print Language.Sprite.Syntax.Front.Abs.ConAppArgs where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.EmptyConAppArgs -> prPrec i 0 (concatD [])
    Language.Sprite.Syntax.Front.Abs.NonEmptyConAppArgs funcappargs -> prPrec i 0 (concatD [doc (showString "("), prt 0 funcappargs, doc (showString ")")])

instance Print Language.Sprite.Syntax.Front.Abs.Annotation where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.Annotation varident rtype -> prPrec i 0 (concatD [doc (showString "/*@"), doc (showString "val"), prt 0 varident, doc (showString ":"), prt 0 rtype, doc (showString "*/")])

instance Print Language.Sprite.Syntax.Front.Abs.Decl where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.RecDecl annotation varident term -> prPrec i 0 (concatD [prt 0 annotation, doc (showString "let"), doc (showString "rec"), prt 0 varident, doc (showString "="), prt 0 term, doc (showString ";")])
    Language.Sprite.Syntax.Front.Abs.AnnotatedDecl annotation varident term -> prPrec i 0 (concatD [prt 0 annotation, doc (showString "let"), prt 0 varident, doc (showString "="), prt 0 term, doc (showString ";")])
    Language.Sprite.Syntax.Front.Abs.UnAnnotatedDecl varident term -> prPrec i 0 (concatD [doc (showString "let"), prt 0 varident, doc (showString "="), prt 0 term, doc (showString ";")])

instance Print Language.Sprite.Syntax.Front.Abs.SwitchCase where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.SwitchCase conident switchcasedataconargs term -> prPrec i 0 (concatD [doc (showString "|"), prt 0 conident, prt 0 switchcasedataconargs, doc (showString "=>"), prt 0 term])

instance Print [Language.Sprite.Syntax.Front.Abs.SwitchCase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.SwitchCaseDataConArgs where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.SwitchCaseNonEmptyDataConArgs funargnames -> prPrec i 0 (concatD [doc (showString "("), prt 0 funargnames, doc (showString ")")])
    Language.Sprite.Syntax.Front.Abs.SwitchCaseEmptyDataConArgs -> prPrec i 0 (concatD [])

instance Print Language.Sprite.Syntax.Front.Abs.IntOp where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.IntPlus -> prPrec i 0 (concatD [doc (showString "+")])
    Language.Sprite.Syntax.Front.Abs.IntMinus -> prPrec i 0 (concatD [doc (showString "-")])
    Language.Sprite.Syntax.Front.Abs.IntMultiply -> prPrec i 0 (concatD [doc (showString "*")])
    Language.Sprite.Syntax.Front.Abs.IntEq -> prPrec i 0 (concatD [doc (showString "==")])
    Language.Sprite.Syntax.Front.Abs.IntLessThan -> prPrec i 0 (concatD [doc (showString "<")])
    Language.Sprite.Syntax.Front.Abs.IntLessOrEqThan -> prPrec i 0 (concatD [doc (showString "<=")])
    Language.Sprite.Syntax.Front.Abs.IntGreaterThan -> prPrec i 0 (concatD [doc (showString ">")])
    Language.Sprite.Syntax.Front.Abs.IntGreaterOrEqThan -> prPrec i 0 (concatD [doc (showString ">=")])

instance Print Language.Sprite.Syntax.Front.Abs.Refinement where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.KnownRefinement varident pred -> prPrec i 0 (concatD [doc (showString "["), prt 0 varident, doc (showString "|"), prt 0 pred, doc (showString "]")])
    Language.Sprite.Syntax.Front.Abs.UnknownRefinement -> prPrec i 0 (concatD [doc (showString "["), doc (showString "?"), doc (showString "]")])
    Language.Sprite.Syntax.Front.Abs.SimpleRefinement -> prPrec i 0 (concatD [])

instance Print Language.Sprite.Syntax.Front.Abs.RType where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.TypeFun funcarg rtype -> prPrec i 0 (concatD [prt 0 funcarg, doc (showString "=>"), prt 0 rtype])
    Language.Sprite.Syntax.Front.Abs.TypeRefined basetype refinement -> prPrec i 0 (concatD [prt 0 basetype, prt 0 refinement])
    Language.Sprite.Syntax.Front.Abs.TypeData varident typedataargs refinement -> prPrec i 0 (concatD [prt 0 varident, prt 0 typedataargs, prt 0 refinement])

instance Print Language.Sprite.Syntax.Front.Abs.TypeDataArg where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.TypeDataArg rtype -> prPrec i 0 (concatD [prt 0 rtype])

instance Print [Language.Sprite.Syntax.Front.Abs.TypeDataArg] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.TypeDataArgs where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.NonEmptyTypeDataArgs typedataargs -> prPrec i 0 (concatD [doc (showString "("), prt 0 typedataargs, doc (showString ")")])
    Language.Sprite.Syntax.Front.Abs.EmptyTypeDataArgs -> prPrec i 0 (concatD [])

instance Print Language.Sprite.Syntax.Front.Abs.DataType where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.DataType varident datatypeargs datatypeconstructors -> prPrec i 0 (concatD [doc (showString "type"), prt 0 varident, prt 0 datatypeargs, doc (showString "="), doc (showString "|"), prt 0 datatypeconstructors, doc (showString ";")])

instance Print [Language.Sprite.Syntax.Front.Abs.DataType] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "\n"), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.DataTypeConstructor where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.DataTypeConstructor conident datatypeconstructorargs datatypeconstructorpredicate -> prPrec i 0 (concatD [prt 0 conident, prt 0 datatypeconstructorargs, prt 0 datatypeconstructorpredicate])

instance Print [Language.Sprite.Syntax.Front.Abs.DataTypeConstructor] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "|"), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.DataTypeConstructorArgs where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.NonEmptyDataTypeConstructorArgs funcargs -> prPrec i 0 (concatD [doc (showString "("), prt 0 funcargs, doc (showString ")")])
    Language.Sprite.Syntax.Front.Abs.EmptyDataTypeConstructorArgs -> prPrec i 0 (concatD [])

instance Print Language.Sprite.Syntax.Front.Abs.DataTypeConstructorPredicate where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.NonEmptyDataTypeConstructorPredicate varident pred -> prPrec i 0 (concatD [doc (showString "=>"), doc (showString "["), prt 0 varident, doc (showString "|"), prt 0 pred, doc (showString "]")])
    Language.Sprite.Syntax.Front.Abs.EmptyDataTypeConstructorPredicate -> prPrec i 0 (concatD [])

instance Print Language.Sprite.Syntax.Front.Abs.DataTypeArgs where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.NonEmptyDataTypeArgs typevarids -> prPrec i 0 (concatD [doc (showString "("), prt 0 typevarids, doc (showString ")")])
    Language.Sprite.Syntax.Front.Abs.EmptyDataTypeArgs -> prPrec i 0 (concatD [])

instance Print Language.Sprite.Syntax.Front.Abs.FuncArg where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.NamedFuncArg varident rtype -> prPrec i 0 (concatD [prt 0 varident, doc (showString ":"), prt 0 rtype])
    Language.Sprite.Syntax.Front.Abs.UnNamedFuncArg rtype -> prPrec i 0 (concatD [doc (showString "_"), doc (showString ":"), prt 0 rtype])

instance Print [Language.Sprite.Syntax.Front.Abs.FuncArg] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.Pred where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.PVar varident -> prPrec i 6 (concatD [prt 0 varident])
    Language.Sprite.Syntax.Front.Abs.PBool constbool -> prPrec i 6 (concatD [prt 0 constbool])
    Language.Sprite.Syntax.Front.Abs.PInt n -> prPrec i 6 (concatD [prt 0 n])
    Language.Sprite.Syntax.Front.Abs.PMeasure measureident measureargs -> prPrec i 6 (concatD [prt 0 measureident, doc (showString "("), prt 0 measureargs, doc (showString ")")])
    Language.Sprite.Syntax.Front.Abs.POr pred1 pred2 -> prPrec i 1 (concatD [prt 1 pred1, doc (showString "||"), prt 2 pred2])
    Language.Sprite.Syntax.Front.Abs.PAnd pred1 pred2 -> prPrec i 2 (concatD [prt 2 pred1, doc (showString "&&"), prt 3 pred2])
    Language.Sprite.Syntax.Front.Abs.PEq pred1 pred2 -> prPrec i 3 (concatD [prt 3 pred1, doc (showString "=="), prt 4 pred2])
    Language.Sprite.Syntax.Front.Abs.PLessThan pred1 pred2 -> prPrec i 4 (concatD [prt 4 pred1, doc (showString "<"), prt 5 pred2])
    Language.Sprite.Syntax.Front.Abs.PLessOrEqThan pred1 pred2 -> prPrec i 4 (concatD [prt 4 pred1, doc (showString "<="), prt 5 pred2])
    Language.Sprite.Syntax.Front.Abs.PGreaterThan pred1 pred2 -> prPrec i 4 (concatD [prt 4 pred1, doc (showString ">"), prt 5 pred2])
    Language.Sprite.Syntax.Front.Abs.PGreaterOrEqThan pred1 pred2 -> prPrec i 4 (concatD [prt 4 pred1, doc (showString ">="), prt 5 pred2])
    Language.Sprite.Syntax.Front.Abs.PPlus pred1 pred2 -> prPrec i 5 (concatD [prt 5 pred1, doc (showString "+"), prt 6 pred2])
    Language.Sprite.Syntax.Front.Abs.PMinus pred1 pred2 -> prPrec i 5 (concatD [prt 5 pred1, doc (showString "-"), prt 6 pred2])
    Language.Sprite.Syntax.Front.Abs.PMultiply pred1 pred2 -> prPrec i 5 (concatD [prt 5 pred1, doc (showString "*"), prt 6 pred2])

instance Print Language.Sprite.Syntax.Front.Abs.MeasureArg where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.MeasureArg pred -> prPrec i 0 (concatD [prt 0 pred])

instance Print [Language.Sprite.Syntax.Front.Abs.MeasureArg] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.BaseType where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.BaseTypeInt -> prPrec i 0 (concatD [doc (showString "int")])
    Language.Sprite.Syntax.Front.Abs.BaseTypeBool -> prPrec i 0 (concatD [doc (showString "bool")])
    Language.Sprite.Syntax.Front.Abs.BaseTypeVar typevarid -> prPrec i 0 (concatD [prt 0 typevarid])

instance Print Language.Sprite.Syntax.Front.Abs.TypeVarId where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.TypeVarId varident -> prPrec i 0 (concatD [doc (showString "'"), prt 0 varident])

instance Print [Language.Sprite.Syntax.Front.Abs.TypeVarId] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.FunArgName where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.FunArgName varident -> prPrec i 0 (concatD [prt 0 varident])

instance Print [Language.Sprite.Syntax.Front.Abs.FunArgName] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Language.Sprite.Syntax.Front.Abs.FuncAppArg where
  prt i = \case
    Language.Sprite.Syntax.Front.Abs.FuncAppArgBool constbool -> prPrec i 0 (concatD [prt 0 constbool])
    Language.Sprite.Syntax.Front.Abs.FuncAppArgInt n -> prPrec i 0 (concatD [prt 0 n])
    Language.Sprite.Syntax.Front.Abs.FuncAppArgVar varident -> prPrec i 0 (concatD [prt 0 varident])

instance Print [Language.Sprite.Syntax.Front.Abs.FuncAppArg] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]
