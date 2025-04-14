{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Sprite.TypeCheck.Run where

import Language.Sprite.Syntax.Front.Abs qualified as Front
import Language.Sprite.Syntax.Convert.FrontToInner qualified as FrontToInner
import Language.Sprite.TypeCheck.Check qualified as Check
import Language.Sprite.TypeCheck.Monad qualified as Check
import Language.Sprite.Syntax qualified as S
import System.Exit (exitFailure)
import qualified Data.Map as Map
import qualified Control.Monad.Foil as Foil
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable (for_)
import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (Bifunctor (second))
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.State (StateT (runStateT))
import qualified Language.Sprite.TypeCheck.Types as S
import qualified Language.Sprite.TypeCheck.Elaboration as Elaboration
import qualified Language.Sprite.Syntax.Inner.Print as Inner
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import qualified Language.Refinements.Constraint as LR
import qualified Language.Refinements.Measure as LR
import qualified Language.Refinements.Run as LR
import Language.Sprite.Syntax.Convert.QualifierToLR (convertQualifier)


runM :: [(Inner.ConIdent, S.Term 'Foil.VoidS)] -> Check.CheckerM a -> IO (Either Text a, Check.CheckerState)
runM constructors = flip runStateT Check.defaultCheckerState
  . flip runReaderT Check.defaultCheckerEnv{Check.dataConstructorsEnv = Map.fromList constructors}
  . runExceptT
  . Check.runCheckerM

vcgen ::
  [(Inner.ConIdent, S.Term 'Foil.VoidS)] ->
  S.Term Foil.VoidS ->
  IO (LR.Constraint, LR.RefinementCheckState)
vcgen constructors term = do
  let
    programType = S.anyIntT
  (elaboratedTerm, _) <- runM constructors (Elaboration.check Foil.emptyScope LR.EmptyEnv term programType)
    >>= \case
      (Left err, _) -> do
        print ("Elaboration errors:" :: Text)
        print err
        exitFailure
      (Right t, _) -> pure t
  -- pPrint $ S.fromTerm elaboratedTerm
  print ("Elaborated term:" :: Text)
  TIO.putStrLn $ Check.showT elaboratedTerm
  (eConstraints, checkerState) <-
    runM constructors $ Check.check Foil.emptyScope LR.EmptyEnv elaboratedTerm programType
  case eConstraints of
    Left err -> do
      print ("Check error:" :: Text)
      print err
      exitFailure
    Right constraints -> pure (constraints, checkerState.refinementCheckState)

convertMeasure :: Front.Measure -> LR.Measure
convertMeasure (Front.Measure (Front.VarIdent fMeasureName) fMeasureType) =
  let
    scopedTyp = S.toTerm Foil.emptyScope Map.empty
      . FrontToInner.mkForAll $ FrontToInner.convertRType fMeasureType
  in LR.mkMeasure fMeasureName scopedTyp


run :: FilePath -> Front.Program -> IO ()
run filePath (Front.Program rawQualifiers rawMeasures dataTypes rawFrontTerm) = do
  let qualifiers = convertQualifier <$> rawQualifiers
  let measures = convertMeasure <$> rawMeasures
  rawInnerTerm <- case FrontToInner.convert rawFrontTerm of
    Right rawInnerTerm -> pure rawInnerTerm
    Left errs -> do
      print errs
      exitFailure
  let
    scopedTerm = S.toTerm Foil.emptyScope Map.empty rawInnerTerm
    scopedConstructors =
      map (second (S.toTerm Foil.emptyScope Map.empty))
      . concatMap FrontToInner.convertDataType
      $ dataTypes
  putStrLn "Raw inner term"
  putStrLn $ Inner.printTree rawInnerTerm
  putStrLn "Raw scoped term"
  print scopedTerm
  (constraints, checkerState) <- vcgen scopedConstructors scopedTerm
  result <- LR.runConstraintsCheck filePath checkerState qualifiers measures constraints
  case result of
    Left err -> do
      print ("Constraints check error:" :: Text)
      print err
      exitFailure
    Right res -> case res of
      LR.Safe {}    -> putStrLn "Safe"
      LR.Unsafe  errs  -> do
        putStrLn "Unsafe: "
        for_ errs $ \e -> putStrLn (T.unpack e)
