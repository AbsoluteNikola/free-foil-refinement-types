module Language.Sprite.TypeCheck.Constraints where

import Language.Sprite.Syntax.Inner.Abs qualified as Inner
import Language.Sprite.Syntax.Convert.InnerToFTR qualified as InnerToFTR
import Language.Fixpoint.Horn.Types qualified as H
import Language.Fixpoint.Types.Sorts qualified as T
import Data.String (fromString)
import Data.Text (Text)
import qualified Language.Sprite.Syntax.Inner.Abs as I


data Constraint
  = CPred Inner.Term Text
  | CAnd [Constraint]
  | CImplication Inner.VarIdent Inner.BaseType Inner.Term Constraint Text
  deriving (Show)

cTrue :: Constraint
cTrue = CAnd []

baseTypeToSort :: I.BaseType -> T.Sort
baseTypeToSort = \case
  I.BaseTypeInt -> T.intSort
  I.BaseTypeBool -> T.boolSort

constraintsToFHT :: Constraint -> Either InnerToFTR.ConvertError (H.Cstr Text)
constraintsToFHT = \case
  CPred p msg -> do
    pred' <- InnerToFTR.convert p
    pure $ H.Head pred' msg
  CAnd cs -> do H.CAnd <$> traverse constraintsToFHT cs
  CImplication (I.VarIdent varId) base p c msg -> do
    p' <- InnerToFTR.convert p
    c' <- constraintsToFHT c
    pure $ H.All
      (H.Bind
        (fromString varId)
        (baseTypeToSort base)
        p'
        msg)
      c'
