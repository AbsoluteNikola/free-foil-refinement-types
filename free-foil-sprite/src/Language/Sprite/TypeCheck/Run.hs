{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Sprite.TypeCheck.Run where

import Language.Sprite.Syntax.Front.Abs qualified as Front
import Language.Sprite.Syntax.Convert.FrontToInner qualified as FrontToInner
import Language.Sprite.TypeCheck.Check qualified as Check
import Language.Sprite.TypeCheck.Constraints qualified as Check
import Language.Sprite.TypeCheck.Monad qualified as Check
import Language.Sprite.Syntax qualified as S
import System.Exit (exitFailure)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Monad.Foil as Foil
import Data.Text (Text)
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Horn.Solve as H
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as F
import qualified Language.Fixpoint.Misc as F
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Data.Foldable (for_)
import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first, Bifunctor (second))
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.State (StateT (runStateT))
import qualified Language.Sprite.TypeCheck.Types as S
import qualified Language.Fixpoint.Horn.Types as F
import qualified Language.Sprite.Syntax.Convert.QualifierToFTR as QualifiersToFTR
import qualified Language.Sprite.TypeCheck.Elaboration as Elaboration
import qualified Language.Sprite.Syntax.Inner.Print as Inner
import Text.Pretty.Simple (pPrint)
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import qualified Language.Fixpoint.Types.Names as FST
import qualified Language.Fixpoint.Types.Sorts as FST

-- TODO: add better errors
instance F.Loc T.Text where
  srcSpan _ = F.dummySpan


runM :: [(Inner.ConIdent, S.Term 'Foil.VoidS)] -> Check.CheckerM a -> IO (Either Text a, Check.CheckerState)
runM constructors = flip runStateT Check.defaultCheckerState
  . flip runReaderT Check.defaultCheckerEnv{Check.dataConstructorsEnv = Map.fromList constructors}
  . runExceptT
  . Check.runCheckerM

vcgen ::
  [F.Qualifier] ->
  [(Inner.ConIdent, S.Term 'Foil.VoidS)] ->
  [(F.Symbol, F.Sort)] ->
  S.Term Foil.VoidS -> IO (Either Text (H.Query Text))
vcgen qualifiers constructors measures term = do
  let
    programType = S.anyIntT
  (elaboratedTerm, _) <- runM constructors (Elaboration.check Foil.emptyScope Check.EmptyEnv term programType)
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
    runM constructors $ Check.check Foil.emptyScope Check.EmptyEnv elaboratedTerm programType
  let
    mkQuery c = do
      c' <- first Check.showT $ Check.constraintsToFHT c
      pure $ H.Query qualifiers checkerState.hornVars c' (HashMap.fromList measures) mempty mempty mempty mempty mempty mempty
  pPrint eConstraints
  pure $ eConstraints >>= mkQuery

config :: FC.Config
config = FC.defConfig
  { FC.metadata = True
  , FC.solver = FC.Z3
  , FC.eliminate = FC.Some -- TODO: understand
  }

checkValid :: FilePath -> H.Query Text -> IO (F.FixResult Text)
checkValid f query = do
  dumpQuery f query
  fmap snd . F.resStatus <$> H.solve config query

dumpQuery :: FilePath -> H.Query Text -> IO ()
dumpQuery f q = do
  putStrLn "BEGIN: Horn VC"
  let smtFile = F.extFileName F.Smt2 f
  F.ensurePath smtFile
  -- need to allow fixpoint cli calls on dumped constraints
  writeFile smtFile (PJ.render . F.toHornSMT $ q)
  putStrLn "END: Horn VC"

convertMeasure :: Front.Measure -> Either Text (FST.Symbol, FST.Sort)
convertMeasure (Front.Measure fMeasureName fMeasureType) = do
  let
    measureName = case FrontToInner.convertVarId fMeasureName of
      (Inner.VarIdent name) -> FST.symbol name
    scopedTyp = S.toTerm Foil.emptyScope Map.empty
      . FrontToInner.mkForAll $ FrontToInner.convertRType fMeasureType
  typSort <- Check.getTypeSort scopedTyp
  pure (FST.symbol measureName, typSort)

run :: FilePath -> Front.Program -> IO ()
run filePath (Front.Program rawQualifiers rawMeasures dataTypes rawFrontTerm) = do
  qualifiers <- case traverse (QualifiersToFTR.convertQualifier filePath) rawQualifiers of
    Right qualifiers -> pure qualifiers
    Left err -> do
      print err
      exitFailure
  measures <- case traverse convertMeasure rawMeasures of
    Right measures -> pure measures
    Left err -> do
      print err
      exitFailure
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
  result <- vcgen qualifiers scopedConstructors measures scopedTerm >>= \case
    Left err -> do
      putStrLn "Type check error: "
      putStrLn (T.unpack err)
      pure $ F.Crash [(err, Nothing)] "VCGen failure"
    Right query -> do
      checkValid filePath query
  case result of
    F.Safe {}    -> putStrLn "Safe"
    F.Unsafe _ errs  -> do
      putStrLn "Unsafe: "
      for_ errs $ \e -> putStrLn (T.unpack e)
    F.Crash _ msg -> putStrLn $  "Crash!: " ++ msg
