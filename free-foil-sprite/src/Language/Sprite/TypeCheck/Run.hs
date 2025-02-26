{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Sprite.TypeCheck.Run where

import Language.Sprite.Syntax.Front.Abs qualified as Front
import Language.Sprite.Syntax.Convert.FrontToInner qualified as FrontToInner
import Language.Sprite.TypeCheck.Check qualified as Check
import Language.Sprite.TypeCheck.Constraints qualified as Check
import Language.Sprite.Syntax qualified as S
import System.Exit (exitFailure)
import qualified Data.Map as Map
import qualified Control.Monad.Foil as Foil
import Data.Text (Text)
import qualified Language.Fixpoint.Horn.Types as H
import qualified Language.Fixpoint.Horn.Solve as H
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as F
import qualified Language.Fixpoint.Misc as F
import qualified Data.Text as T
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Data.Foldable (for_)
import Control.Monad.Trans.Except (runExceptT)
import Data.Bifunctor (first)
import Control.Monad.Reader (ReaderT(runReaderT))
import qualified Language.Sprite.TypeCheck.Types as S

-- TODO: add better errors
instance F.Loc T.Text where
  srcSpan _ = F.dummySpan


vcgen :: S.Term Foil.VoidS -> IO (Either Text (H.Query Text))
vcgen term = do
  let
    programType = S.anyIntT
  eConstraints <- flip runReaderT Check.defaultCheckerDebugEnv . runExceptT . Check.runCheckerM  $ Check.check Foil.emptyScope Check.EmptyEnv term programType
  let
    mkQuery c = do
      c' <- first Check.showT $ Check.constraintsToFHT c
      pure $ H.Query [] [] c' mempty mempty mempty mempty mempty mempty mempty
  pure $ eConstraints >>= mkQuery

config :: FC.Config
config = FC.defConfig
  { FC.metadata = True
  , FC.solver = FC.Z3
  , FC.json = True
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
  writeFile smtFile (PJ.render . F.pprint $ q)
  putStrLn "END: Horn VC"


run :: FilePath -> Front.Term -> IO ()
run filePath rawFrontTerm = do
  rawInnerTerm <- case FrontToInner.convert rawFrontTerm of
    Right rawInnerTerm -> pure rawInnerTerm
    Left errs -> do
      print errs
      exitFailure
  let scopedTerm =  S.toTerm Foil.emptyScope Map.empty rawInnerTerm
  -- pPrint rawFrontTerm
  print scopedTerm
  result <- vcgen scopedTerm >>= \case
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
