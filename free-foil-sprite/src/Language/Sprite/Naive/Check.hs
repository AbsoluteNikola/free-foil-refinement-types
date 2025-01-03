{-# LANGUAGE TypeApplications #-}
module Language.Sprite.Naive.Check where

import Data.Map.Strict qualified as M

import Language.Sprite.Syntax.Abs
import Control.Monad.Except (ExceptT)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (MonadReader(..), asks)
import Language.Sprite.Naive.Constraints
import Language.Sprite.Naive.Predicates
import Text.Pretty.Simple (pShow, pPrint)
import Language.Sprite.Naive.Substitution (substType, substPred)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)

newtype Env = Env {unEnv :: M.Map VarIdent RType }

newtype CheckerM a = CheckerM {runCheckerM :: ReaderT Env (ExceptT Text IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Text, MonadIO, MonadReader Env)

pShowT :: Show a => a -> Text
pShowT = TL.toStrict . pShow

showT :: Show a => a -> Text
showT = T.pack . show

debugPrint :: Show a => a -> CheckerM ()
debugPrint = pPrint

debugPrintT :: Text -> CheckerM ()
debugPrintT = liftIO . TIO.putStrLn

emptyEnv :: Env
emptyEnv = Env M.empty

withExtendedEnv :: VarIdent -> RType -> CheckerM a -> CheckerM a
withExtendedEnv varId typ = local (Env . M.insert varId typ . unEnv)

lookupEnv :: VarIdent -> CheckerM (Maybe RType)
lookupEnv varId = asks (M.lookup varId . unEnv)

subtype :: RType -> RType -> CheckerM Constraint

{- | [Sub-Base]
     (v::t) => q[w := v]
     -------------------
     b{v:p} <= b{w:q}
 -}
subtype lt@(TypeRefined lb leftVarId leftPredicate) rt@(TypeRefined rb rightVarId rightPredicate)
  | lb /= rb = throwError
    $ "Invalid subtyping. Different refined base: " <> pShowT lb <> " and " <> pShowT rb
  | otherwise = do
    let
      implMsg = "Refinement subtype error: (v::t) => q[w := v]. Where v="
        <> showT leftVarId
        <> ", w=" <> showT rightVarId
      predMsg = "Refinement subtype error: q[w := v]. Where w="
        <> showT rightVarId <> " and v=" <> showT leftVarId
      predSubst = CPred (substPred rightVarId (PVar leftVarId) rightPredicate) predMsg
    pure $
      buildImplicationFromType implMsg leftVarId lt predSubst


{- | [Sub-Fun]

    s2 <: s1    x2:s2 |- t1[x1:=x2] <: t2
    -------------------------------------
    x1:s1 -> t1 <: x2:s2 -> t2
-}
subtype
  (TypeFun (NamedFuncArg leftFunArgId leftFunArgT) (ScopedRType leftFunReturnType))
  (TypeFun (NamedFuncArg rightFunArgId rightFunArgT) (ScopedRType rightFunReturnType)) = do
  --  s2 <: s1
  argSubtypingConstrains <- subtype rightFunArgT leftFunArgT
  let
    -- t1[x1:=x2]
    leftFunReturnTypeSubstituted = substType leftFunArgId (PVar rightFunArgId) leftFunReturnType
  --  t1[x1:=x2] <: t2
  returnTypesSubtypingConstraints <- subtype leftFunReturnTypeSubstituted rightFunReturnType
  -- x2:s2 |- t1[x1:=x2] <: t2
  let
    implMsg = "Function subtype error: x2:s2 |- t1[x1:=x2] <: t2. Where x2="  <> showT rightFunArgId
    returnTypesConstraints =
      buildImplicationFromType implMsg rightFunArgId rightFunArgT returnTypesSubtypingConstraints
  pure $ CAnd [argSubtypingConstrains, returnTypesConstraints]

subtype t1 t2 = throwError
    $ "Invalid subtyping. Different types" <> pShowT t1 <> "\nand\n" <> pShowT t2

check :: Term -> RType -> CheckerM Constraint
check currentTerm currentType = case currentTerm of
  {- [Chk-Lam]
    G, x:s |- e <== t
    --------------------------
    G |- \x.e <== x:s -> t
 -}
  Fun argId (ScopedTerm funcBody) -> case currentType of
    TypeRefined{} -> throwError $
      "Function type should be Function, not: " <> pShowT currentType
    TypeFun (NamedFuncArg argId' funcArgType) (ScopedRType funcReturnType)
      | argId' /= argId -> throwError $
        "Function argument identifier and identifier in its type are different:  "
        <> "type arg name = " <> pShowT argId' <> ", "
        <> "func arg name = " <> pShowT argId
      | otherwise-> do
        -- G, x:s |- e <== t
        bodyCheckConstraints <- withExtendedEnv argId funcArgType $
          check funcBody funcReturnType
        pure $ buildImplicationFromType "Chk-Lam"
          argId funcArgType bodyCheckConstraints

  {- [Chk-Let]
      G |- e1 ==> t1        G, x:t1 |- e2 <== t2
      -------------------------------------------
          G |- let x = e1 in e2 <== t2
  -}
  Let decl (ScopedTerm body) -> do
    -- G |- e1 ==> t1
    (synthsDeclConstraint, declType) <- synthsDecl decl
    checkBodyConstraints <- withExtendedEnv (getDeclVarId decl) declType $
      check body currentType
    -- G, x:t1 |- e2 <== t2
    let implicationConstraint = buildImplicationFromType "Chk-Let" (getDeclVarId decl) declType checkBodyConstraints
    pure $ CAnd [synthsDeclConstraint, implicationConstraint]


  {- [Chk-Syn]
    G |- e ==> s        G |- s <: t
    ----------------------------------[Chk-Syn]
              G |- e <== t
  -}
  term -> do
    -- G |- e ==> s
    (synthsConstraints, termType) <- synths term
    --  G |- s <: t
    subtypingConstraints <- subtype termType currentType
    pure $ CAnd [synthsConstraints, subtypingConstraints]

synthsDecl :: Decl -> CheckerM (Constraint, RType)
synthsDecl currentDecl = case currentDecl of
  {- [Syn-Ann]
   G |- e <== t
   -----------------
   G |- e:t => t
  -}
  AnnotatedDecl (Annotation annVarId annType) (PlainDecl declVarId declTerm)
    | annVarId /= declVarId ->
        throwError
          $ "Annotated identifier and declaration identifier should match. "
          <> "Annotation: " <> pShowT annVarId
          <> "Declaration: " <> pShowT declVarId
    | otherwise -> do
      -- G |- e <== t
      checkConstraints <- check declTerm annType
      pure (checkConstraints, annType)
  -- Just try to synthesize term without help of annotation
  UnAnnotatedDecl (PlainDecl _ declTerm) -> do
    synths declTerm

synths :: Term -> CheckerM (Constraint, RType)
synths currentTerm = case currentTerm of
  {- [Syn-Var]
   -----------------
    G |- x ==> G(x)
  -}
  Var varId -> lookupEnv varId >>= \case
    Just typ -> pure (cTrue, typ)
    Nothing -> throwError $ "Unbound variable: " <> pShowT varId

  {- [Syn-Con]
   -----------------
    G |- x ==> G(x)
  -}
  ConstInt x -> pure (cTrue, constIntT x)

  {- [Syn-Con] + [Syn-App] for bin operation
  -}
  Op leftTerm op rightTerm  -> do
    let
      opTypes = intBinOpTypes op
    lc <- check (funcAppArgToTerm leftTerm) opTypes.leftArgT
    rc <- check (funcAppArgToTerm rightTerm) opTypes.rightArgT
    let
      -- the same as two applications (Syn-App)
      substitutedReturnType =
        substType opTypes.leftArgId (funcArgTermToPred leftTerm) opTypes.resultType
          & substType opTypes.rightArgId (funcArgTermToPred rightTerm)
    pure (CAnd [lc, rc], substitutedReturnType)

  {- [Syn-App]
   G |- e ==> x:s -> t       G |- y <== s
   --------------------------------------
   G |- e y ==> t[x := y]
  -}
  App funcTerm argTerm -> do
    -- G |- e ==> x:s -> t
    (funcConstraints, funcType) <- synths funcTerm
    case funcType of
      TypeFun (NamedFuncArg argId argT) (ScopedRType returnT) -> do
        -- G |- y <== s
        argConstraints <- check (funcAppArgToTerm argTerm) argT
        let
          -- y aka application argument in ANF form
          substitutedReturnType = substType argId (funcArgTermToPred argTerm) returnT
        pure (CAnd [funcConstraints, argConstraints], substitutedReturnType)
      _ -> throwError $
        "Application to non-function: " <> pShowT funcTerm
        <> "\nActual type: " <> pShowT funcType

  other -> throwError $ "Can't synths case: " <> pShowT other

funcAppArgToTerm :: FuncAppArg -> Term
funcAppArgToTerm = \case
  FuncAppArgInt x -> ConstInt x
  FuncAppArgVar v -> Var v

getDeclVarId :: Decl -> VarIdent
getDeclVarId = \case
  AnnotatedDecl _ (PlainDecl varId _) -> varId
  UnAnnotatedDecl (PlainDecl varId _) -> varId
