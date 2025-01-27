{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}

module Language.Sprite.TypeCheck.Check where
import Control.Monad.Foil qualified as F
import Control.Monad.Foil.Internal qualified as F
import Language.Sprite.Syntax
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Language.Sprite.TypeCheck.Constraints
import qualified Data.Text.Lazy as TL
import Text.Pretty.Simple (pShow, pPrint)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Free.Foil (AST(Var))
-- import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import Data.Bifunctor (bimap)

deriving instance Functor (F.NameMap n)

newtype Env scope = Env {unEnv :: F.NameMap scope (Term scope, F.Scope scope) }

newtype CheckerM a = CheckerM {runCheckerM :: (ExceptT Text IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Text, MonadIO)

withExtendedEnv :: (F.DExt i o) => Env i -> F.NameBinder i o -> Term o -> F.Scope o -> (Env o -> CheckerM a) -> CheckerM a
withExtendedEnv (Env env) varBinder typ typScope action = do
  let env' =  F.addNameBinder varBinder (typ, typScope) (bimap F.sink F.sink <$> env)
  action (Env env')

lookupEnv :: Env o -> F.Name o -> (Term o, F.Scope o)
lookupEnv env varId =
  F.lookupName varId $ unEnv env

pShowT :: Show a => a -> Text
pShowT = TL.toStrict . pShow

showT :: Show a => a -> Text
showT = T.pack . show

debugPrint :: Show a => a -> CheckerM ()
debugPrint = pPrint

debugPrintT :: Text -> CheckerM ()
debugPrintT = liftIO . TIO.putStrLn

subtype :: Term o -> Term o -> CheckerM Constraint

{- | [Sub-Base]
     (v::t) => q[w := v]
     -------------------
     b{v:p} <= b{w:q}
 -}
subtype lt@(TypeRefined lb leftVarId _leftPredicate) (TypeRefined rb rightVarId rightPredicate)
  | lb /= rb = throwError
    $ "Invalid subtyping. Different refined base: " <> pShowT lb <> " and " <> pShowT rb
  | otherwise = do undefined
    -- let
    --   implMsg = "Refinement subtype error: (v::t) => q[w := v]. Where v="
    --     <> showT leftVarId
    --     <> ", w=" <> showT rightVarId
    --   predMsg = "Refinement subtype error: q[w := v]. Where w="
    --     <> showT rightVarId <> " and v=" <> showT leftVarId
    --   predSubst = CPred (substPred rightVarId (PVar leftVarId) rightPredicate) predMsg
    -- pure $
    --   buildImplicationFromType implMsg leftVarId lt predSubst

subtype _ _ = error "unimplemented"

synths ::
  (F.Distinct i, F.ExtEndo i) =>
  F.Scope i ->
  Env i ->
  Term i ->
  CheckerM (Constraint, (Term o, F.Scope o))
synths scope env currentTerm = case currentTerm of
  {- [Syn-Var]
   -----------------
    G |- x ==> G(x)
  -}
  Var varId -> do
    let (typ, typScope) = lookupEnv env varId
    pure (cTrue, (typ, typScope))

  {- [Syn-Con]
   -----------------
    G |- x ==> G(x)
  -}
  ConstInt x -> do
    pure (cTrue, (F.sink $ constIntT x, F.emptyScope))

  {- [Syn-App]
   G |- e ==> x:s -> t       G |- y <== s
   --------------------------------------
   G |- e y ==> t[x := y]
  -}
  App funcTerm argTerm -> do
     -- G |- e ==> x:s -> t
    (funcConstraints, (funcType, funcTypeScope)) <- synths scope env funcTerm
    case funcType of
      TypeFun varPattern varType returnType -> do undefined
    undefined

  _ -> error "unimplemented case"

{-
constIntP :: VarIdent -> Integer -> Pred
constIntP varId x = PEq (PVar varId) (PInt x)

constIntT :: Integer -> RType
constIntT x = TypeRefined BaseTypeInt v $ constIntP v x
  where v = VarIdent "constInt"

-}
constIntT :: Integer -> Term F.VoidS
constIntT x = F.withFreshBinder F.emptyScope $
  \binder -> TypeRefined Inner.BaseTypeInt (PatternVar binder) (PEq (Var (F.nameOf binder)) (ConstInt x))
