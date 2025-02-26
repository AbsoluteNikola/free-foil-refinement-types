module Language.Sprite.Naive.Constraints where

import Language.Sprite.Syntax.Front.Abs (Pred (PVar), VarIdent (..), BaseType(..), RType(..))
import Language.Fixpoint.Horn.Types qualified as H
import Language.Fixpoint.Types.Sorts qualified as T
import Language.Sprite.Naive.Predicates (predToFTR)
import Data.String (fromString)
import Language.Sprite.Naive.Substitution (substPred)
import Data.Text (Text)

-- TODO: add meta information about error
data Constraint
  = CPred Pred Text
  | CAnd [Constraint]
  | CImplication VarIdent BaseType Pred Constraint Text
  deriving (Show)

cTrue :: Constraint
cTrue = CAnd []

--  id:b[pred] => constraint. See page 17
buildImplicationFromType :: Text -> VarIdent -> RType -> Constraint -> Constraint
buildImplicationFromType msg varId typ constraint = case typ of
  TypeRefined base typVarId p ->
    CImplication varId base (substPred typVarId (PVar varId) p) constraint msg
  TypeFun{} -> constraint

constraintsToFHT :: Constraint -> H.Cstr Text
constraintsToFHT = \case
  CPred p msg -> H.Head (H.Reft $ predToFTR p) msg
  CAnd cs -> H.CAnd $ constraintsToFHT <$> cs
  CImplication (VarIdent varId) base p c msg ->
    H.All
      (H.Bind
        (fromString varId)
        (baseTypeToSort base)
        (H.Reft $ predToFTR p)
        msg)
      (constraintsToFHT c)

baseTypeToSort :: BaseType -> T.Sort
baseTypeToSort = \case
  BaseTypeInt -> T.FInt
