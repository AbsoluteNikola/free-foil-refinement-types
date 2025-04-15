module Language.Sprite.TypeCheck.Constraints where

import Language.Sprite.Syntax.Inner.Abs qualified as Inner
import Language.Sprite.Syntax.Convert.InnerToFTR qualified as InnerToFTR
import Language.Fixpoint.Horn.Types qualified as H
import Language.Fixpoint.Types.Sorts qualified as T
import Data.String (fromString)
import Data.Text (Text)
import qualified Language.Sprite.Syntax.Inner.Abs as I
import qualified Language.Fixpoint.Types as F


data Constraint
  = CPred Inner.Term Text
  | CAnd [Constraint]
  | CImplication Inner.VarIdent Inner.Term Inner.Term Constraint Text
  deriving (Show)

cTrue :: Constraint
cTrue = CAnd []

baseTypeToSort :: I.Term ->  Maybe T.Sort
baseTypeToSort = \case
  I.BaseTypeInt -> pure T.intSort
  I.BaseTypeBool -> pure T.boolSort
  I.BaseTypeVar (I.Var (I.VarIdent v))-> pure . T.FObj . F.symbol $ v
  _ -> Nothing

constraintsToFHT :: Constraint -> Either InnerToFTR.ConvertError (H.Cstr Text)
constraintsToFHT = \case
  CPred p msg -> do
    pred' <- InnerToFTR.convert p
    pure $ H.Head pred' msg
  CAnd cs -> do H.CAnd <$> traverse constraintsToFHT cs
  CImplication (I.VarIdent varId) base p c msg -> do
    p' <- InnerToFTR.convert p
    c' <- constraintsToFHT c
    base' <- case baseTypeToSort base of
      Just b -> pure b
      Nothing -> Left $ InnerToFTR.UnknownBaseType base
    pure $ H.All
      (H.Bind
        (fromString varId)
        base'
        p'
        msg)
      c'
