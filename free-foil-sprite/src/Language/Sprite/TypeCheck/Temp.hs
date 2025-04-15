module Language.Sprite.TypeCheck.Temp where
import Language.Sprite.Syntax.Inner.Abs
import Language.Sprite.Syntax qualified as S
import Control.Monad.Free.Foil qualified as F
import Control.Monad.Foil qualified as F
import qualified Data.Map as Map


-- >>> t
-- ∀ x0 : ∀ x1 : x2 : ' x0 [x2 | true] => x3 : int [x3 | true] => x4 : ' x1 [x4 | true] => ' x0 [x5 | true]
t :: S.Term F.VoidS
t = S.toTerm F.emptyScope Map.empty $ TypeForall (PatternVar (VarIdent "x3")) (ScopedTerm (TypeForall (PatternVar (VarIdent "x4")) (ScopedTerm (TypeFun (PatternVar (VarIdent "x5")) (TypeRefined (BaseTypeVar (Var (VarIdent "x3"))) (PatternVar (VarIdent "x5")) (ScopedTerm (Boolean ConstTrue))) (ScopedTerm (TypeFun (PatternVar (VarIdent "x6")) (TypeRefined BaseTypeInt (PatternVar (VarIdent "x6")) (ScopedTerm (Boolean ConstTrue))) (ScopedTerm (TypeFun (PatternVar (VarIdent "x7")) (TypeRefined (BaseTypeVar (Var (VarIdent "x4"))) (PatternVar (VarIdent "x7")) (ScopedTerm (Boolean ConstTrue))) (ScopedTerm (TypeRefined (BaseTypeVar (Var (VarIdent "x3"))) (PatternVar (VarIdent "x8")) (ScopedTerm (Boolean ConstTrue))))))))))))

t1 = undefined
z = subst
