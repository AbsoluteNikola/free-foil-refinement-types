-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Language.

module Language.Sprite.Syntax.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Language.Sprite.Syntax.Abs

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

instance Print Language.Sprite.Syntax.Abs.VarIdent where
  prt _ (Language.Sprite.Syntax.Abs.VarIdent i) = doc $ showString i
instance Print Language.Sprite.Syntax.Abs.Term where
  prt i = \case
    Language.Sprite.Syntax.Abs.ConstInt n -> prPrec i 0 (concatD [prt 0 n])
    Language.Sprite.Syntax.Abs.Var varident -> prPrec i 0 (concatD [prt 0 varident])
    Language.Sprite.Syntax.Abs.Let decl scopedterm -> prPrec i 0 (concatD [prt 0 decl, prt 0 scopedterm])
    Language.Sprite.Syntax.Abs.Fun varident scopedterm -> prPrec i 0 (concatD [doc (showString "("), prt 0 varident, doc (showString ")"), doc (showString "=>"), doc (showString "{"), prt 0 scopedterm, doc (showString "}")])
    Language.Sprite.Syntax.Abs.App term funcapparg -> prPrec i 0 (concatD [prt 0 term, doc (showString "("), prt 0 funcapparg, doc (showString ")")])
    Language.Sprite.Syntax.Abs.Op funcapparg1 intop funcapparg2 -> prPrec i 0 (concatD [prt 0 funcapparg1, prt 0 intop, prt 0 funcapparg2])

instance Print Language.Sprite.Syntax.Abs.Annotation where
  prt i = \case
    Language.Sprite.Syntax.Abs.Annotation varident rtype -> prPrec i 0 (concatD [doc (showString "/*@"), doc (showString "val"), prt 0 varident, doc (showString ":"), prt 0 rtype, doc (showString "*/")])

instance Print Language.Sprite.Syntax.Abs.PlainDecl where
  prt i = \case
    Language.Sprite.Syntax.Abs.PlainDecl varident term -> prPrec i 0 (concatD [doc (showString "let"), prt 0 varident, doc (showString "="), prt 0 term, doc (showString ";")])

instance Print Language.Sprite.Syntax.Abs.Decl where
  prt i = \case
    Language.Sprite.Syntax.Abs.AnnotatedDecl annotation plaindecl -> prPrec i 0 (concatD [prt 0 annotation, prt 0 plaindecl])
    Language.Sprite.Syntax.Abs.UnAnnotatedDecl plaindecl -> prPrec i 0 (concatD [prt 0 plaindecl])

instance Print [Language.Sprite.Syntax.Abs.Decl] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Language.Sprite.Syntax.Abs.IntOp where
  prt i = \case
    Language.Sprite.Syntax.Abs.IntPlus -> prPrec i 0 (concatD [doc (showString "+")])
    Language.Sprite.Syntax.Abs.IntMinus -> prPrec i 0 (concatD [doc (showString "-")])
    Language.Sprite.Syntax.Abs.IntMultiply -> prPrec i 0 (concatD [doc (showString "*")])

instance Print Language.Sprite.Syntax.Abs.RType where
  prt i = \case
    Language.Sprite.Syntax.Abs.TypeRefined basetype varident pred -> prPrec i 0 (concatD [prt 0 basetype, doc (showString "["), prt 0 varident, doc (showString "|"), prt 0 pred, doc (showString "]")])
    Language.Sprite.Syntax.Abs.TypeFun funcarg scopedrtype -> prPrec i 0 (concatD [prt 0 funcarg, doc (showString "=>"), prt 0 scopedrtype])

instance Print Language.Sprite.Syntax.Abs.ScopedRType where
  prt i = \case
    Language.Sprite.Syntax.Abs.ScopedRType rtype -> prPrec i 0 (concatD [prt 0 rtype])

instance Print Language.Sprite.Syntax.Abs.FuncArg where
  prt i = \case
    Language.Sprite.Syntax.Abs.NamedFuncArg varident rtype -> prPrec i 0 (concatD [prt 0 varident, doc (showString ":"), prt 0 rtype])

instance Print Language.Sprite.Syntax.Abs.Pred where
  prt i = \case
    Language.Sprite.Syntax.Abs.PVar varident -> prPrec i 0 (concatD [prt 0 varident])
    Language.Sprite.Syntax.Abs.PTrue -> prPrec i 0 (concatD [doc (showString "true")])
    Language.Sprite.Syntax.Abs.PFalse -> prPrec i 0 (concatD [doc (showString "false")])
    Language.Sprite.Syntax.Abs.PInt n -> prPrec i 0 (concatD [prt 0 n])
    Language.Sprite.Syntax.Abs.PEq pred1 pred2 -> prPrec i 0 (concatD [prt 0 pred1, doc (showString "=="), prt 0 pred2])
    Language.Sprite.Syntax.Abs.PLessThan pred1 pred2 -> prPrec i 0 (concatD [prt 0 pred1, doc (showString "<"), prt 0 pred2])
    Language.Sprite.Syntax.Abs.PLessOrEqThan pred1 pred2 -> prPrec i 0 (concatD [prt 0 pred1, doc (showString "<="), prt 0 pred2])
    Language.Sprite.Syntax.Abs.PPlus pred1 pred2 -> prPrec i 0 (concatD [prt 0 pred1, doc (showString "+"), prt 0 pred2])
    Language.Sprite.Syntax.Abs.PMinus pred1 pred2 -> prPrec i 0 (concatD [prt 0 pred1, doc (showString "-"), prt 0 pred2])
    Language.Sprite.Syntax.Abs.PMultiply pred1 pred2 -> prPrec i 0 (concatD [prt 0 pred1, doc (showString "*"), prt 0 pred2])

instance Print Language.Sprite.Syntax.Abs.Pattern where
  prt i = \case
    Language.Sprite.Syntax.Abs.PatternVar varident -> prPrec i 0 (concatD [prt 0 varident])

instance Print Language.Sprite.Syntax.Abs.ScopedTerm where
  prt i = \case
    Language.Sprite.Syntax.Abs.ScopedTerm term -> prPrec i 0 (concatD [prt 0 term])

instance Print Language.Sprite.Syntax.Abs.BaseType where
  prt i = \case
    Language.Sprite.Syntax.Abs.BaseTypeInt -> prPrec i 0 (concatD [doc (showString "int")])

instance Print Language.Sprite.Syntax.Abs.FuncAppArg where
  prt i = \case
    Language.Sprite.Syntax.Abs.FuncAppArgInt n -> prPrec i 0 (concatD [prt 0 n])
    Language.Sprite.Syntax.Abs.FuncAppArgVar varident -> prPrec i 0 (concatD [prt 0 varident])
