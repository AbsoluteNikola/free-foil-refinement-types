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

makeQuery :: RefinementCheckState -> [Qualifier] -> [Measure] -> Constraint -> Either T.Text (LF.Query T.Text)
makeQuery refinementCheckState quals measures constraint = do
  convertedConstraints <- first (T.pack . show) $ constraintsToLF constraint
  let
    convertedQuals = convertQualifier <$> quals
    convertedMeasures = measures <&> \m -> (LF.symbol m.measureName, m.sort)
    hornVars = unHornVar <$> refinementCheckState.hornVars
  pure $ LF.Query convertedQuals hornVars convertedConstraints (HashMap.fromList convertedMeasures) mempty mempty mempty mempty mempty mempty

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

runConstraintsCheck :: FilePath -> RefinementCheckState -> [Qualifier] -> [Measure] -> Constraint -> IO (Either T.Text Result)
runConstraintsCheck filePath refinementCheckState quals measures constraint = do
  let eQuery = makeQuery refinementCheckState quals measures constraint
  case eQuery of
    Left err -> pure (Left err)
    Right query -> do
      dumpQuery filePath query
      Right <$> checkValid filePath query
