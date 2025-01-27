module Language.Sprite.TypeCheck.Constraints where

import Language.Sprite.Syntax.Inner.Abs qualified as Inner
-- import Language.Fixpoint.Horn.Types qualified as H
-- import Language.Fixpoint.Types.Sorts qualified as T
-- import Language.Sprite.Naive.Predicates (predToFTR)
-- import Data.String (fromString)
-- import Language.Sprite.Naive.Substitution (substPred)
import Data.Text (Text)

-- TODO: add meta information about error
data Constraint
  = CPred Inner.Term Text
  | CAnd [Constraint]
  | CImplication Inner.VarIdent Inner.BaseType Inner.Term Constraint Text
  deriving (Show)

cTrue :: Constraint
cTrue = CAnd []
