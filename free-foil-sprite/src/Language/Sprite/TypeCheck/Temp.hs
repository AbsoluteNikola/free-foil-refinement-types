module X where
import Language.Sprite.Syntax.Inner.Abs
import Language.Sprite.Syntax (toTerm)
import Control.Monad.Free.Foil qualified as F
import Control.Monad.Foil qualified as F
import qualified Data.Map as Map

-- >>> toTerm F.emptyScope Map.empty z
-- let x0 = /*@ ∀ x0 : ∀ x1 : x2 : ' x1 [x2 | true] => x3 : int [x3 | true] => x4 : ' x0 [x4 | true] => ' x1 [x5 | true] */ /\ x0 : /\ x1 : (x2) =>
-- {
--   (x3) =>
--   {
--     (x4) =>
--     {
--       x2
--     }
--   }
-- };
-- let x1 = /*@ ∀ x1 : x2 : ' x1 [x2 | true] => ' x1 [x3 | true] */ /\ x1 : (x2) =>
-- {
--   let x3 = x0 [' x1 [x3 | true]] ['' tmp0 [x3 | true]] (x2);
--   let x4 = x3 (0);
--   x4 (0)
-- };
-- let x2 = /*@ x2 : int [x2 | (0 <= x2)] => int [x3 | (0 <= x3)] */ (x2) =>
-- {
--   let x3 = x0 [int [x3 | (0 <= x3)]] ['' tmp2 [x3 | true]] (x2);
--   let x4 = x3 (0);
--   let x5 = x4 (false);
--   x5
-- };
-- 0
z =
    Let
    ( PatternVar
        ( VarIdent "x0" )
    )
    ( Ann
        ( TypeForall
            ( PatternVar
                ( VarIdent "x0" )
            )
            ( ScopedTerm
                ( TypeForall
                    ( PatternVar
                        ( VarIdent "x1" )
                    )
                    ( ScopedTerm
                        ( TypeFun
                            ( PatternVar
                                ( VarIdent "x2" )
                            )
                            ( TypeRefined
                                ( BaseTypeVar
                                    ( Var
                                        ( VarIdent "x1" )
                                    )
                                )
                                ( PatternVar
                                    ( VarIdent "x2" )
                                )
                                ( ScopedTerm ( Boolean ConstTrue ) )
                            )
                            ( ScopedTerm
                                ( TypeFun
                                    ( PatternVar
                                        ( VarIdent "x3" )
                                    )
                                    ( TypeRefined BaseTypeInt
                                        ( PatternVar
                                            ( VarIdent "x3" )
                                        )
                                        ( ScopedTerm ( Boolean ConstTrue ) )
                                    )
                                    ( ScopedTerm
                                        ( TypeFun
                                            ( PatternVar
                                                ( VarIdent "x4" )
                                            )
                                            ( TypeRefined
                                                ( BaseTypeVar
                                                    ( Var
                                                        ( VarIdent "x0" )
                                                    )
                                                )
                                                ( PatternVar
                                                    ( VarIdent "x4" )
                                                )
                                                ( ScopedTerm ( Boolean ConstTrue ) )
                                            )
                                            ( ScopedTerm
                                                ( TypeRefined
                                                    ( BaseTypeVar
                                                        ( Var
                                                            ( VarIdent "x1" )
                                                        )
                                                    )
                                                    ( PatternVar
                                                        ( VarIdent "x5" )
                                                    )
                                                    ( ScopedTerm ( Boolean ConstTrue ) )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        ( TAbs
            ( PatternVar
                ( VarIdent "x0" )
            )
            ( ScopedTerm
                ( TAbs
                    ( PatternVar
                        ( VarIdent "x1" )
                    )
                    ( ScopedTerm
                        ( Fun
                            ( PatternVar
                                ( VarIdent "x2" )
                            )
                            ( ScopedTerm
                                ( Fun
                                    ( PatternVar
                                        ( VarIdent "x3" )
                                    )
                                    ( ScopedTerm
                                        ( Fun
                                            ( PatternVar
                                                ( VarIdent "x4" )
                                            )
                                            ( ScopedTerm
                                                ( Var
                                                    ( VarIdent "x2" )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    ( ScopedTerm
        ( Let
            ( PatternVar
                ( VarIdent "x1" )
            )
            ( Ann
                ( TypeForall
                    ( PatternVar
                        ( VarIdent "x1" )
                    )
                    ( ScopedTerm
                        ( TypeFun
                            ( PatternVar
                                ( VarIdent "x2" )
                            )
                            ( TypeRefined
                                ( BaseTypeVar
                                    ( Var
                                        ( VarIdent "x1" )
                                    )
                                )
                                ( PatternVar
                                    ( VarIdent "x2" )
                                )
                                ( ScopedTerm ( Boolean ConstTrue ) )
                            )
                            ( ScopedTerm
                                ( TypeRefined
                                    ( BaseTypeVar
                                        ( Var
                                            ( VarIdent "x1" )
                                        )
                                    )
                                    ( PatternVar
                                        ( VarIdent "x3" )
                                    )
                                    ( ScopedTerm ( Boolean ConstTrue ) )
                                )
                            )
                        )
                    )
                )
                ( TAbs
                    ( PatternVar
                        ( VarIdent "x1" )
                    )
                    ( ScopedTerm
                        ( Fun
                            ( PatternVar
                                ( VarIdent "x2" )
                            )
                            ( ScopedTerm
                                ( Let
                                    ( PatternVar
                                        ( VarIdent "x3" )
                                    )
                                    ( App
                                        ( TApp
                                            ( TApp
                                                ( Var
                                                    ( VarIdent "x0" )
                                                )
                                                ( TypeRefined
                                                    ( BaseTypeVar
                                                        ( Var
                                                            ( VarIdent "x1" )
                                                        )
                                                    )
                                                    ( PatternVar
                                                        ( VarIdent "x3" )
                                                    )
                                                    ( ScopedTerm ( Boolean ConstTrue ) )
                                                )
                                            )
                                            ( TypeRefined
                                                ( BaseTypeTempVar
                                                    ( VarIdent "tmp0" )
                                                )
                                                ( PatternVar
                                                    ( VarIdent "x3" )
                                                )
                                                ( ScopedTerm ( Boolean ConstTrue ) )
                                            )
                                        )
                                        ( Var
                                            ( VarIdent "x2" )
                                        )
                                    )
                                    ( ScopedTerm
                                        ( Let
                                            ( PatternVar
                                                ( VarIdent "x4" )
                                            )
                                            ( App
                                                ( Var
                                                    ( VarIdent "x3" )
                                                )
                                                ( ConstInt 0 )
                                            )
                                            ( ScopedTerm
                                                ( App
                                                    ( Var
                                                        ( VarIdent "x4" )
                                                    )
                                                    ( ConstInt 0 )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            ( ScopedTerm
                ( Let
                    ( PatternVar
                        ( VarIdent "x2" )
                    )
                    ( Ann
                        ( TypeFun
                            ( PatternVar
                                ( VarIdent "x2" )
                            )
                            ( TypeRefined BaseTypeInt
                                ( PatternVar
                                    ( VarIdent "x2" )
                                )
                                ( ScopedTerm
                                    ( OpExpr
                                        ( ConstInt 0 ) LessOrEqOp
                                        ( Var
                                            ( VarIdent "x2" )
                                        )
                                    )
                                )
                            )
                            ( ScopedTerm
                                ( TypeRefined BaseTypeInt
                                    ( PatternVar
                                        ( VarIdent "x3" )
                                    )
                                    ( ScopedTerm
                                        ( OpExpr
                                            ( ConstInt 0 ) LessOrEqOp
                                            ( Var
                                                ( VarIdent "x3" )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                        ( Fun
                            ( PatternVar
                                ( VarIdent "x2" )
                            )
                            ( ScopedTerm
                                ( Let
                                    ( PatternVar
                                        ( VarIdent "x3" )
                                    )
                                    ( App
                                        ( TApp
                                            ( TApp
                                                ( Var
                                                    ( VarIdent "x0" )
                                                )
                                                ( TypeRefined BaseTypeInt
                                                    ( PatternVar
                                                        ( VarIdent "x3" )
                                                    )
                                                    ( ScopedTerm
                                                        ( OpExpr
                                                            ( ConstInt 0 ) LessOrEqOp
                                                            ( Var
                                                                ( VarIdent "x3" )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                            ( TypeRefined
                                                ( BaseTypeTempVar
                                                    ( VarIdent "tmp2" )
                                                )
                                                ( PatternVar
                                                    ( VarIdent "x3" )
                                                )
                                                ( ScopedTerm ( Boolean ConstTrue ) )
                                            )
                                        )
                                        ( Var
                                            ( VarIdent "x2" )
                                        )
                                    )
                                    ( ScopedTerm
                                        ( Let
                                            ( PatternVar
                                                ( VarIdent "x4" )
                                            )
                                            ( App
                                                ( Var
                                                    ( VarIdent "x3" )
                                                )
                                                ( ConstInt 0 )
                                            )
                                            ( ScopedTerm
                                                ( Let
                                                    ( PatternVar
                                                        ( VarIdent "x5" )
                                                    )
                                                    ( App
                                                        ( Var
                                                            ( VarIdent "x4" )
                                                        ) ( Boolean ConstFalse )
                                                    )
                                                    ( ScopedTerm
                                                        ( Var
                                                            ( VarIdent "x5" )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                    ( ScopedTerm
                        ( ConstInt 0 )
                    )
                )
            )
        )
    )
