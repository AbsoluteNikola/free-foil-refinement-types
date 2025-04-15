{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Language.Sprite.TypeCheck.Monad where
import qualified Control.Monad.Foil as F
import Data.Text (Text)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Language.Sprite.Syntax
import Control.Monad.Except (MonadError)
import Control.Monad.Reader.Class (MonadReader (ask), local)
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

data CheckerDebugEnv = CheckerDebugEnv
  { currentRule :: Text
  , offset :: Text
  }

data CheckerState = CheckerState
  -- refinement check env
  { hornVarIndex :: Int
  , hornVars :: [H.Var Text]
  -- elaboration env
  -- Завтрашний я: как будто хранить констреинты не надо, можно сразу в типы делать подстановку и потом ловить ошибки, ну кароч как в спрайте сделано
  , typeConstraints :: [(Inner.VarIdent, Term F.VoidS)]
  , typeVarIndex :: Int
  }

defaultCheckerDebugEnv :: CheckerDebugEnv
defaultCheckerDebugEnv = CheckerDebugEnv "" ""

defaultCheckerState :: CheckerState
defaultCheckerState = CheckerState
  { hornVarIndex = 0
  , hornVars = []
  , typeConstraints = []
  , typeVarIndex = 0
  }

newtype CheckerM a = CheckerM {runCheckerM :: (ExceptT Text (ReaderT CheckerDebugEnv (StateT CheckerState IO))) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Text, MonadReader CheckerDebugEnv, MonadState CheckerState, MonadIO)

data Env n where
    EmptyEnv :: Env F.VoidS
    NonEmptyEnv :: F.DExt i o => Env i -> F.NameBinder i o -> Term i -> Env o

withExtendedEnv :: (F.DExt i o) => Env i -> F.NameBinder i o -> Term i -> (Env o -> CheckerM a) -> CheckerM a
withExtendedEnv env binder typ action = do
  debugPrintT $ "add binder x" <> showT binder <> " with type " <> showT typ
  let env' = NonEmptyEnv env binder typ
  action env'

withExtendedEnv' :: (F.Distinct i, F.DExt i o) => Env i -> F.NameBinderList i o -> Term i -> (Env o -> CheckerM a) -> CheckerM a
withExtendedEnv' env binders typ action =
  case binders of
    F.NameBinderListEmpty -> action env
    F.NameBinderListCons binder otherBinders -> do
      case (F.assertDistinct binder, F.assertExt binder) of
        (F.Distinct, F.Ext) -> case (F.assertDistinct otherBinders, F.assertExt otherBinders) of
          (F.Distinct, F.Ext) -> do
            env' <- withExtendedEnv env binder typ pure
            withExtendedEnv'
              env'
              otherBinders
              (F.sink typ)
              action

lookupEnv :: Env o -> F.Name o -> Term o
lookupEnv env varId = case env of
  EmptyEnv -> error $ "impossible case, no var in env: " <> show varId
  NonEmptyEnv env' binder term ->
    if F.nameOf binder == varId
      then F.sink term
      else
        case F.unsinkName binder varId of
          Nothing -> error $ "impossible case. If we didn't found var in bigger scoped map. It should be in smaller scope " <> show varId
          Just varId' -> F.sink $ lookupEnv env' varId'

envToList :: Env o -> [(F.Name o, Term o)]
envToList env = case env of
  EmptyEnv -> []
  NonEmptyEnv env' binder term ->
    (F.nameOf binder, F.sink term)
      : map (bimap F.sink  F.sink) (envToList env')

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
  res <- local (\debugEnv -> CheckerDebugEnv rule (debugEnv.offset <> "  ")) action
  liftIO . TIO.putStrLn $ denv.offset <> "  " <> rule <> " done"
  pure res

mkFreshHornVar :: [FP.Sort] -> CheckerM Inner.VarIdent
mkFreshHornVar sorts = do
  newIndex <- gets (.hornVarIndex)
  let varName = "k" <> show newIndex
  let hv = H.HVar (FP.symbol varName) sorts "fake"
  modify $ \s ->
     s
     { hornVarIndex = newIndex + 1
     , hornVars = hv : s.hornVars
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
