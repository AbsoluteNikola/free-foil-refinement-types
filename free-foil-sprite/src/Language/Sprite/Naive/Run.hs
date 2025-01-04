{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- TODO: remove after better errors
module Language.Sprite.Naive.Run where

import Language.Sprite.Naive.Check
import Language.Sprite.Syntax.Front.Abs (Term, RType (TypeRefined), BaseType(..), Pred (PTrue))
import qualified Language.Fixpoint.Horn.Types   as H
import qualified Language.Fixpoint.Horn.Solve   as H
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT(runReaderT))
import Language.Sprite.Naive.Constraints (constraintsToFHT)
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC
import qualified Language.Fixpoint.Utils.Files as F
import qualified Language.Fixpoint.Misc as F
import qualified Text.PrettyPrint.HughesPJ.Compat as PJ
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (for_)

-- TODO: add better errors
instance F.Loc T.Text where
  srcSpan _ = F.dummySpan

vcgen :: Term -> IO (Either Text (H.Query Text))
vcgen term = do
  let programType = TypeRefined BaseTypeInt "program" PTrue
  eConstraints <- runExceptT . flip runReaderT emptyEnv . runCheckerM $ check term programType
  let
    mkQuery c =
      H.Query [] [] (constraintsToFHT c) mempty mempty mempty mempty mempty mempty mempty
  pure $ mkQuery <$> eConstraints

config :: FC.Config
config = FC.defConfig
  { FC.metadata = True
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

run :: FilePath -> Term -> IO ()
run f term = do
  result <- vcgen term >>= \case
    Left err -> do
      putStrLn "Type check error: "
      putStrLn (T.unpack err)
      pure $ F.Crash [(err, Nothing)] "VCGen failure"
    Right query -> do
      checkValid f query
  case result of
    F.Safe {}    -> putStrLn "Safe"
    F.Unsafe _ errs  -> do
      putStrLn "Unsafe: "
      for_ errs $ \e -> putStrLn (T.unpack e)
    F.Crash _ msg -> putStrLn $  "Crash!: " ++ msg
