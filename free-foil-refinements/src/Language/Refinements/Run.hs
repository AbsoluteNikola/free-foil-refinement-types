{-# OPTIONS_GHC -Wno-orphans #-}
module Language.Refinements.Run where
import qualified Language.Fixpoint.Types.Config as LF
import qualified Language.Fixpoint.Horn.Types as LF
import qualified Data.Text as T
import qualified Language.Fixpoint.Types as LF
import qualified Language.Fixpoint.Utils.Files as LF
import qualified Language.Fixpoint.Misc as LF
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Language.Refinements.Constraint
import Language.Refinements.Measure
import Data.Biapplicative (first)
import Control.Monad.State (MonadState, gets)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HashMap
import qualified Language.Fixpoint.Horn.Solve as LF

config :: LF.Config
config = LF.defConfig
  { LF.metadata = True
  , LF.solver = LF.Z3
  , LF.eliminate = LF.Some
  }

instance LF.Loc T.Text where
  srcSpan _ = LF.dummySpan

data Result = Safe | Unsafe [T.Text]

toResult :: LF.FixResult T.Text -> Result
toResult = \case
  c@LF.Crash{} -> error (show c)
  LF.Safe{} -> Safe
  LF.Unsafe _ errors -> Unsafe errors

makeQuery :: MonadState RefinementCheckState m => [Qualifier] -> [Measure] -> Constraint -> m (Either T.Text (LF.Query T.Text))
makeQuery quals measures constraints = do
  let
    convertedConstraints = first (T.pack . show) $ constraintsToLF constraints
    convertedQuals = convertQualifier <$> quals
    convertedMeasures = measures <&> \m -> (LF.symbol m.measureName, m.sort)
  hornVars <- gets (map unHornVar . (.hornVars))
  pure $ do
    c' <- convertedConstraints
    pure $ LF.Query convertedQuals hornVars c' (HashMap.fromList convertedMeasures) mempty mempty mempty mempty mempty mempty
  where

checkValid :: FilePath -> LF.Query T.Text -> IO Result
checkValid f query = do
  dumpQuery f query
  toResult . fmap snd . LF.resStatus <$> LF.solve config query

dumpQuery :: FilePath -> LF.Query T.Text -> IO ()
dumpQuery f q = do
  putStrLn "BEGIN: Horn VC"
  let smtFile = LF.extFileName LF.Smt2 f
  LF.ensurePath smtFile
  -- need to allow fixpoint cli calls on dumped constraints
  writeFile smtFile (PJ.render . LF.toHornSMT $ q)
  putStrLn "END: Horn VC"
