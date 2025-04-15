module Language.Sprite.TypeCheck.Temp where
import Language.Sprite.Syntax.Inner.Abs
import Language.Sprite.Syntax qualified as S
import Control.Monad.Foil qualified as F
import qualified Data.Map as Map
import Language.Sprite.TypeCheck.Constraints qualified as C
import Language.Refinements.TypeSignature qualified as LR
import Language.Refinements.Constraint qualified as LR

-- >>> t
-- ∀ x0 : list (' x0 [x1 | true]) [x1 | true]
-- >>> C.getTypeSort t
-- FAbs 0 (FApp (FTC (TC "list" )) (FVar 0)))
-- >>> LR.toTypeSignature t
-- ForallType (Id "x0") (DataType (Id "list") [DataTypeArg (VarType (Id "x0"))])
-- >>> LR.typeToSort $ LR.toTypeSignature t
-- FAbs 0 (FApp (FTC (TC "list")) (FTC (TC "x0")))
t :: S.Term F.VoidS
t = S.toTerm F.emptyScope Map.empty $
  TypeForall
    (PatternVar (VarIdent "x0"))
    ( ScopedTerm
      (TypeData "list"
        ( NonEmptyTypeDataArgs
          [TypeDataArg $
            TypeRefined (BaseTypeVar (Var "x0")) (PatternVar "x1")
            (ScopedTerm (Boolean ConstTrue))
          ])
        (PatternVar "x1")
        (ScopedTerm (Boolean ConstTrue))))
