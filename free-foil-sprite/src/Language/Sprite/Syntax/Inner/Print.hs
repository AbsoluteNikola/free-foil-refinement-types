-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Language.

module Language.Sprite.Syntax.Inner.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Language.Sprite.Syntax.Inner.Abs

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

instance Print Language.Sprite.Syntax.Inner.Abs.VarIdent where
  prt _ (Language.Sprite.Syntax.Inner.Abs.VarIdent i) = doc $ showString i
instance Print Language.Sprite.Syntax.Inner.Abs.Term where
  prt i = \case
    Language.Sprite.Syntax.Inner.Abs.ConstInt n -> prPrec i 0 (concatD [prt 0 n])
    Language.Sprite.Syntax.Inner.Abs.Boolean constbool -> prPrec i 0 (concatD [prt 0 constbool])
    Language.Sprite.Syntax.Inner.Abs.Var varident -> prPrec i 0 (concatD [prt 0 varident])
    Language.Sprite.Syntax.Inner.Abs.If term1 term2 term3 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 term1, doc (showString ")"), doc (showString "{"), prt 0 term2, doc (showString "}"), doc (showString "else"), doc (showString "{"), prt 0 term3, doc (showString "}")])
    Language.Sprite.Syntax.Inner.Abs.Let pattern_ term scopedterm -> prPrec i 0 (concatD [doc (showString "let"), prt 0 pattern_, doc (showString "="), prt 0 term, doc (showString ";"), prt 0 scopedterm])
    Language.Sprite.Syntax.Inner.Abs.Fun pattern_ scopedterm -> prPrec i 0 (concatD [doc (showString "("), prt 0 pattern_, doc (showString ")"), doc (showString "=>"), doc (showString "{"), prt 0 scopedterm, doc (showString "}")])
    Language.Sprite.Syntax.Inner.Abs.App term1 term2 -> prPrec i 0 (concatD [prt 0 term1, doc (showString "("), prt 0 term2, doc (showString ")")])
    Language.Sprite.Syntax.Inner.Abs.Ann term1 term2 -> prPrec i 0 (concatD [doc (showString "/*@"), prt 0 term1, doc (showString "*/"), prt 0 term2])
    Language.Sprite.Syntax.Inner.Abs.OpExpr term1 op term2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 term1, prt 0 op, prt 0 term2, doc (showString ")")])
    Language.Sprite.Syntax.Inner.Abs.TypeRefined basetype pattern_ scopedterm -> prPrec i 0 (concatD [prt 0 basetype, doc (showString "["), prt 0 pattern_, doc (showString "|"), prt 0 scopedterm, doc (showString "]")])
    Language.Sprite.Syntax.Inner.Abs.TypeFun pattern_ term scopedterm -> prPrec i 0 (concatD [prt 0 pattern_, doc (showString ":"), prt 0 term, doc (showString "=>"), prt 0 scopedterm])

instance Print Language.Sprite.Syntax.Inner.Abs.ConstBool where
  prt i = \case
    Language.Sprite.Syntax.Inner.Abs.ConstTrue -> prPrec i 0 (concatD [doc (showString "true")])
    Language.Sprite.Syntax.Inner.Abs.ConstFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print Language.Sprite.Syntax.Inner.Abs.Op where
  prt i = \case
    Language.Sprite.Syntax.Inner.Abs.EqOp -> prPrec i 0 (concatD [doc (showString "==")])
    Language.Sprite.Syntax.Inner.Abs.LessOrEqOp -> prPrec i 0 (concatD [doc (showString "<=")])
    Language.Sprite.Syntax.Inner.Abs.LessOp -> prPrec i 0 (concatD [doc (showString "<")])
    Language.Sprite.Syntax.Inner.Abs.GreaterOrEqOp -> prPrec i 0 (concatD [doc (showString ">=")])
    Language.Sprite.Syntax.Inner.Abs.GreaterOp -> prPrec i 0 (concatD [doc (showString ">")])
    Language.Sprite.Syntax.Inner.Abs.PlusOp -> prPrec i 0 (concatD [doc (showString "+")])
    Language.Sprite.Syntax.Inner.Abs.MinusOp -> prPrec i 0 (concatD [doc (showString "-")])
    Language.Sprite.Syntax.Inner.Abs.MultiplyOp -> prPrec i 0 (concatD [doc (showString "*")])
    Language.Sprite.Syntax.Inner.Abs.AndOp -> prPrec i 0 (concatD [doc (showString "&&")])
    Language.Sprite.Syntax.Inner.Abs.OrOp -> prPrec i 0 (concatD [doc (showString "||")])

instance Print Language.Sprite.Syntax.Inner.Abs.Pattern where
  prt i = \case
    Language.Sprite.Syntax.Inner.Abs.PatternVar varident -> prPrec i 0 (concatD [prt 0 varident])

instance Print Language.Sprite.Syntax.Inner.Abs.ScopedTerm where
  prt i = \case
    Language.Sprite.Syntax.Inner.Abs.ScopedTerm term -> prPrec i 0 (concatD [prt 0 term])

instance Print Language.Sprite.Syntax.Inner.Abs.BaseType where
  prt i = \case
    Language.Sprite.Syntax.Inner.Abs.BaseTypeInt -> prPrec i 0 (concatD [doc (showString "int")])
    Language.Sprite.Syntax.Inner.Abs.BaseTypeBool -> prPrec i 0 (concatD [doc (showString "bool")])

instance Print Language.Sprite.Syntax.Inner.Abs.VarBinding where
  prt i = \case
    Language.Sprite.Syntax.Inner.Abs.VarBinding varident term -> prPrec i 0 (concatD [prt 0 varident, doc (showString ":"), prt 0 term])

instance Print [Language.Sprite.Syntax.Inner.Abs.VarBinding] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]
