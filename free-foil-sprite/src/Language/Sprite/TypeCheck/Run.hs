module Language.Sprite.TypeCheck.Run where

import Language.Sprite.Syntax.Front.Abs qualified as Front
import Language.Sprite.Syntax.Convert qualified as Convert
import Language.Sprite.Syntax qualified as S
import System.Exit (exitFailure)
import qualified Data.Map as Map
import qualified Control.Monad.Foil as Foil
import Text.Pretty.Simple (pPrint)

run :: FilePath -> Front.Term -> IO ()
run _filePath rawFrontTerm = do
  rawInnerTerm <- case Convert.convert rawFrontTerm of
    Right rawInnerTerm -> pure rawInnerTerm
    Left errs -> do
      print errs
      exitFailure
  let scopedTerm =  S.toTerm Foil.emptyScope Map.empty rawInnerTerm
  pPrint rawFrontTerm
  print scopedTerm
  undefined
