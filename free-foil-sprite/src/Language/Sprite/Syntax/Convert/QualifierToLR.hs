module Language.Sprite.Syntax.Convert.QualifierToLR (convertQualifier) where
import qualified Language.Refinements.Measure as LR
import qualified Control.Monad.Foil as Foil
import qualified Data.Map as Map
import qualified Language.Sprite.Syntax as S
import qualified Language.Sprite.Syntax.Front.Abs as Front
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import qualified Language.Sprite.FreeFoilConfig as Inner
import qualified Language.Sprite.Syntax.Convert.FrontToInner as FrontToInner

convertQualifier :: Front.Qualifier -> LR.Qualifier
convertQualifier (Front.Qualifier (Front.VarIdent qualName) rawArgs rawPred)
  = go Map.empty Foil.emptyScope rawArgs []
  where
    go ::
      (Foil.Distinct n) =>
      Map.Map Inner.VarIdent (Foil.Name n) ->
      Foil.Scope n ->
      [Front.QualifierArg] ->
      [(String, S.Term Foil.VoidS)] ->
      LR.Qualifier
    go names scope [] convertedArgs
      = LR.mkQualifier qualName convertedArgs
        (S.toTerm scope names $ FrontToInner.convertPredicate rawPred)

    go names scope (Front.QualifierArg (Front.VarIdent rawArgName) typ:otherArgs) convertedArgs
      = Foil.withFreshBinder scope $ \nameBinder -> case (Foil.assertDistinct nameBinder, Foil.assertExt nameBinder) of
        (Foil.Distinct, Foil.Ext) ->
          let
            scope' = Foil.extendScope nameBinder scope
            convertedTyp = S.toTerm  Foil.emptyScope Map.empty (FrontToInner.convertRType typ)
            names' = Map.insert (Inner.VarIdent rawArgName) (Foil.nameOf nameBinder) (Foil.sink <$> names)
            (Inner.VarIdent foilArgName) = Inner.intToVarIdent $ Foil.nameId $ Foil.nameOf nameBinder
          in go names' scope' otherArgs (convertedArgs ++ [(foilArgName, convertedTyp)])
