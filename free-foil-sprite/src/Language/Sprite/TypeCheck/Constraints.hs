module Language.Sprite.TypeCheck.Constraints where

import Data.Text (Text)
import Language.Sprite.TypeCheck.Monad (CheckerM)
import qualified Control.Monad.Foil as F
import qualified Control.Monad.Foil.Internal as F
import Language.Sprite.Syntax
import qualified Language.Refinements.Constraint as LR


-- В фреймворк
buildImplicationFromType' :: F.Distinct o => Text -> F.Scope o -> F.Name o -> Term o -> LR.Constraint -> CheckerM LR.Constraint
buildImplicationFromType' msg scope argVarId typ constraint =
   pure $ LR.сImplicationFromType scope (F.UnsafeNameBinder argVarId)  typ constraint msg
