{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Sprite.TypeCheck.Interface where
import Data.Text (Text)
import qualified Language.Fixpoint.Horn.Types as LF
import Control.Monad.State (MonadState(..), gets, modify)
import qualified Language.Fixpoint.Types.Names as LF
import qualified Language.Fixpoint.Types.Sorts as LF
import qualified Language.Fixpoint.Horn.Types as LT
import Control.Monad.IO.Class (MonadIO (liftIO))

-- env mode here

data Env


-- sorts
data Sort = FunSort Sort Sort | IntSort | BoolSort | TypeSort [Sort]
  deriving (Show)

toLTSort :: Sort -> LF.Sort
toLTSort = undefined

class IsPredicate a where
  isUnknown :: a -> Bool

-- change Unknown predicate on horn variables

fresh :: a
fresh = undefined

singletonT :: a
singletonT = undefined

class HasType a where
  getSort :: a -> Sort

newtype HornVar = HornVar (LF.Var Text)
newtype HornVarName = HornVarName String

data RefinementCheckState = RefinementCheckState
  { nextHornVarIndex :: Int
  , hornVars :: [HornVar]
  }

mkFreshHornVar :: MonadState RefinementCheckState m => [Sort] -> m HornVarName
mkFreshHornVar sorts = do
  newIndex <- gets (.nextHornVarIndex)
  let varName = "k" <> show newIndex
  let hv = LF.HVar (LF.symbol varName) (toLTSort <$> sorts) "fake"
  modify $ \s ->
     s
     { nextHornVarIndex = newIndex + 1
     , hornVars = HornVar hv : s.hornVars
     }
  pure $ HornVarName varName

-- Constraints

-- TODO: вынести в отдельный синтаксис
data Pred = P
  deriving (Show)

data Constraint
  = CPred Pred Text
  | CAnd [Constraint]
  | CImplication String Sort Pred Constraint Text
  deriving (Show)

class Monad m => MonadConstraints m where
  addConstraint :: Constraint -> m ()
  getConstraint :: m Constraint  -- Returns the captured constraint


instance (Monad m, MonadState Constraint m) => MonadConstraints m where
  addConstraint newC = do
    oldC <- get
    put $ case oldC of
      CAnd cs -> CAnd (cs ++ [newC])  -- Append to existing CAnd
      _       -> CAnd [oldC, newC]    -- Combine into a new CAnd
  getConstraint = get

cTrue :: Constraint
cTrue = CAnd []

-- equality to build implication from type
сImplication :: MonadConstraints m => m ()
сImplication = undefined

cPred :: MonadConstraints m => Pred -> Text -> m ()
cPred p msg = addConstraint $ CPred p msg

cAnd :: MonadConstraints m => [m a] -> m [a]
cAnd = sequenceA

constraintsToLF :: Constraint -> Either Text (LT.Cstr Text)
constraintsToLF = undefined

-- common measures and qualifiers

-- Measure и Qualifier можно вынести отдельно со своим синтаксисом, так как
-- они по сути не взаимодействую на прямую с целевым языком
data Measure

data Qualifier

data Result = Safe | Unsafe Text

checkConstraints :: (MonadIO m, MonadState RefinementCheckState m) => [Measure] -> [Qualifier] -> Constraint -> m (Either Text Result)
checkConstraints _ _ _ = do
  _ <- gets id
  liftIO undefined
