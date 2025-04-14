{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Language.Sprite.TypeCheck.Monad where
import qualified Control.Monad.Foil as F
import qualified Control.Monad.Foil.Internal as F
import Data.Text (Text)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Language.Sprite.Syntax
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader.Class (MonadReader (ask), local, asks)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text.Lazy as TL
import Text.Pretty.Simple (pShow, pShowNoColor)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.State (StateT)
import Control.Monad.State (MonadState, gets, modify)
import Language.Fixpoint.Horn.Types qualified as H
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import qualified Language.Fixpoint.Types.Sorts as FP
import qualified Language.Fixpoint.Types as FP
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as Map
import qualified Language.Refinements.Constraint as LR

data CheckerEnv = CheckerEnv
  { currentRule :: Text
  , offset :: Text
  , dataConstructorsEnv :: Map.Map Inner.ConIdent (Term F.VoidS)
  }

data CheckerState = CheckerState
  -- refinement check env
  { refinementCheckState :: LR.RefinementCheckState
  -- elaboration env
  , typeVarIndex :: Int
  }

defaultCheckerEnv :: CheckerEnv
defaultCheckerEnv = CheckerEnv "" "" Map.empty

defaultCheckerState :: CheckerState
defaultCheckerState = CheckerState
  { refinementCheckState = LR.emptyRefinementCheckState
  , typeVarIndex = 0
  }

newtype CheckerM a = CheckerM {runCheckerM :: (ExceptT Text (ReaderT CheckerEnv (StateT CheckerState IO))) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Text, MonadReader CheckerEnv, MonadState CheckerState, MonadIO)

type Env = LR.Env TermSig Pattern

lookupConstructor :: Inner.ConIdent -> CheckerM (Term F.VoidS)
lookupConstructor conName = do
  constructors <- asks (.dataConstructorsEnv)
  case Map.lookup conName constructors of
    Just typ -> pure typ
    Nothing -> throwError $ "Unknown constructor: " <> showT conName

pShowT :: Show a => a -> Text
pShowT = TL.toStrict . pShow

showT :: Show a => a -> Text
showT = T.pack . show

debugPrint :: Show a => a -> CheckerM ()
debugPrint value = do
  debugEnv <- ask
  let finalText = T.unlines . ("":). fmap (debugEnv.offset <>) . T.lines . TL.toStrict . pShowNoColor $ value
  liftIO . TIO.putStrLn $ debugEnv.offset <> debugEnv.currentRule <> " " <> finalText

debugPrintT :: Text -> CheckerM ()
debugPrintT value = do
  debugEnv <- ask
  liftIO . TIO.putStrLn $ debugEnv.offset <> debugEnv.currentRule <> " " <> value

withRule :: Text -> CheckerM a -> CheckerM a
withRule rule action = do
  denv <- ask
  liftIO . TIO.putStrLn $ denv.offset <> "  " <> rule
  res <- local (\debugEnv -> debugEnv{offset = debugEnv.offset <> "  "}) action
  liftIO . TIO.putStrLn $ denv.offset <> "  " <> rule <> " done"
  pure res

mkFreshHornVar :: [FP.Sort] -> CheckerM Inner.VarIdent
mkFreshHornVar sorts = do
  newIndex <- gets (.refinementCheckState.nextHornVarIndex)
  let varName = "k" <> show newIndex
  let hv = H.HVar (FP.symbol varName) sorts "fake"
  modify $ \s ->
     s
     { refinementCheckState = s.refinementCheckState
       { LR.hornVars = LR.HornVar hv : s.refinementCheckState.hornVars
       ,   LR.nextHornVarIndex = newIndex + 1
       }
     }
  pure $ Inner.VarIdent varName

-- returns 'tmp76 where tmp76 uniq var id in program
mkFreshTempTypVar :: CheckerM (Term F.VoidS)
mkFreshTempTypVar = do
  newIndex <- gets (.typeVarIndex)
  let varName = Inner.VarIdent $ "tmp" <> show newIndex
  modify $ \s ->
     s
     { typeVarIndex = newIndex + 1
     }
  pure $ F.withFreshBinder F.emptyScope $ \newBinder ->
      TypeRefined (BaseTypeTempVar varName) (PatternVar newBinder) (Boolean Inner.ConstTrue)

getNameBinderFromPattern :: Pattern i o -> F.NameBinder i o
getNameBinderFromPattern (PatternVar binder) = binder

getRawVarIdFromPattern :: Pattern i o -> Inner.VarIdent
getRawVarIdFromPattern varPat = case fromPattern varPat of
  Inner.PatternVar v -> v
  _ -> error "getRawVarIdFromPattern should be called on PatternVar"
