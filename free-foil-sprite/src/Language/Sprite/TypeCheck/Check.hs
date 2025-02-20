{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Sprite.TypeCheck.Check where
import Control.Monad.Foil qualified as F
import Control.Monad.Free.Foil qualified as F
import Language.Sprite.Syntax
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Language.Sprite.TypeCheck.Constraints
import qualified Data.Text.Lazy as TL
import Text.Pretty.Simple (pShow, pShowNoColor)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import Language.Sprite.TypeCheck.Predicates
import Control.Monad.Reader (ask, MonadReader (local), ReaderT)

data Env n where
    EmptyEnv :: Env F.VoidS
    NonEmptyEnv :: F.DExt i o => Env i -> F.NameBinder i o -> Term i -> Env o

data CheckerDebugEnv = CheckerDebugEnv
  { currentRule :: Text
  , offset :: Text
  }

defaultCheckerDebugEnv :: CheckerDebugEnv
defaultCheckerDebugEnv = CheckerDebugEnv "" ""

newtype CheckerM a = CheckerM {runCheckerM :: (ExceptT Text (ReaderT CheckerDebugEnv IO)) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Text, MonadReader CheckerDebugEnv, MonadIO)

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

subtype :: F.Distinct i => F.Scope i -> Term  i -> Term i -> CheckerM Constraint

{- | [Sub-Base]
  Γ ⊢ ∀ (v : t). p => q[w := v]
  ————————————————————————————— Sub-Base
  Γ ⊢ b{v:p} <: b{w:q}
 -}
subtype scope lt@(TypeRefined lb leftVarPat@(PatternVar v) _) (TypeRefined rb rightVarPat@(PatternVar w) rightPredicate)
  | lb /= rb = throwError
    $ "Invalid subtyping. Different refined base: " <> pShowT lb <> " and " <> pShowT rb
  | otherwise = withRule "[Sub-Base]" $ do
      case (F.assertDistinct leftVarPat, F.assertExt leftVarPat) of
        (F.Distinct, F.Ext) -> do
          let
            implMsg = "Refinement subtype error: (v::t) => q[w := v]. Where v="
              <> showT leftVarPat
              <> ", w=" <> showT rightVarPat
            predMsg = "Refinement subtype error: q[w := v]. Where w="
              <> showT rightVarPat <> " and v=" <> showT leftVarPat
            scope' = F.extendScopePattern leftVarPat scope
            subst = F.addRename (F.sink F.identitySubst) w (F.nameOf v)
            rightPredicate' = F.substitute scope' subst rightPredicate
            conclusionPred = CPred (fromTerm rightPredicate') predMsg
          buildImplicationFromType implMsg scope' leftVarPat (F.sink lt) conclusionPred

{- | [Sub-Fun]

    s2 <: s1    x2:s2 |- t1[x1:=x2] <: t2
    -------------------------------------
    x1:s1 -> t1 <: x2:s2 -> t2
-}
subtype scope
  (TypeFun leftFunArgPat leftFunArgT leftFunRetT)
  (TypeFun rightFunArgPat rightFunArgT rightFunRetT) = withRule "[Sub-Fun]" $ do
  --  s2 <: s1
  debugPrintT "calling subtype"
  debugPrintT "left type (right arg): " >> debugPrint rightFunArgT
  debugPrintT "right type (left arg): " >> debugPrint leftFunArgT
  argSubtypingConstrains <- subtype scope rightFunArgT leftFunArgT
  case (F.assertDistinct rightFunArgPat, F.assertExt rightFunArgPat) of
    (F.Distinct, F.Ext) -> do
      let
        scope' = F.extendScopePattern rightFunArgPat scope
        leftFunRetTSubst =
          F.addRename
            (F.sink F.identitySubst)
            (getNameBinderFromPattern leftFunArgPat)
            (F.nameOf (getNameBinderFromPattern rightFunArgPat))
        -- t1[x1:=x2]
        leftFunRetTypeSubstituted = F.substitute scope' leftFunRetTSubst leftFunRetT
      -- x2:s2 |- t1[x1:=x2] <: t2
      debugPrintT "calling subtype"
      debugPrintT "left type (left ret type): " >> debugPrint leftFunRetTypeSubstituted
      debugPrintT "right type (right ret type): " >> debugPrint rightFunRetT
      returnTypesSubtypingConstraints <- subtype scope' leftFunRetTypeSubstituted rightFunRetT
      let
        implMsg = "Function subtype error: x2:s2 |- t1[x1:=x2] <: t2. Where x2="  <> showT rightFunArgPat
      debugPrintT $ "rightArgPat = " <> showT rightFunArgPat <> ", leftFunRetT' = " <> showT leftFunRetT
      returnTypesConstraints <- buildImplicationFromType implMsg scope' rightFunArgPat (F.sink rightFunArgT) returnTypesSubtypingConstraints
      pure $ CAnd [argSubtypingConstrains, returnTypesConstraints]

subtype _ lt rt = throwError $
  "can't subtype:\n"
  <> "left type: " <> showT lt <> "\n"
  <> "right type: " <> showT rt

check :: (F.DExt F.VoidS i) =>
  F.Scope i ->
  Env i ->
  -- | term to check
  Term i ->
  -- | type to check with
  Term i ->
  CheckerM Constraint
-- check scope env currentTerm currentType = debugPrintT "check:\ntype:" >> debugPrint currentType >> debugPrintT "\nterm:" >> debugPrint currentTerm >> debugPrintT "\n== check end ==" >> case currentTerm of
check scope env currentTerm currentType = case currentTerm of
  {- [Chk-Lam]
  G, x:s |- e[y := x] <== t
  --------------------------
  G |- \y.e <== x:s -> t
  -}
  Fun varPattern body -> withRule "[Chk-Lam]" $ case currentType of
    TypeFun typeFunArgIdPat@(PatternVar typeFunArgIdBinder) argType returnType -> do
        case  (F.assertExt typeFunArgIdPat, F.assertDistinct typeFunArgIdPat) of
          (F.Ext, F.Distinct) -> do
            let
              scope' = F.extendScopePattern typeFunArgIdPat scope
              bodySubst =
                F.addRename
                  (F.sink F.identitySubst)
                  (getNameBinderFromPattern varPattern)
                  (F.nameOf typeFunArgIdBinder)
              bodySubstituted = F.substitute scope' bodySubst body

            bodyCheckConstraints :: Constraint <- withExtendedEnv env typeFunArgIdBinder argType $ \env' ->
              check scope' env' bodySubstituted returnType
            debugPrintT $ "Arg type: " <> showT argType
            buildImplicationFromType "Chk-Lam" scope' typeFunArgIdPat (F.sink argType) bodyCheckConstraints
    _ -> throwError $ "Function type should be Function, not: " <> pShowT currentType

  {- [Chk-Let]
    G |- e1 ==> t1        G, x:t1 |- e2 <== t2
    -------------------------------------------
        G |- let x = e1 in e2 <== t2
  -}
  Let newVarPat newVarValue body -> withRule "[Chk-Let]" $ do
    -- G |- e1 ==> t1
    debugPrintT $  "synths " <> showT newVarPat
    (newVarConstraints, newVarType) <- synths scope env newVarValue
    case (F.assertDistinct newVarPat, F.assertExt newVarPat) of
      (F.Distinct, F.Ext) ->  do
        --  x:t1 |- e2 <== t2
        withExtendedEnv env (getNameBinderFromPattern newVarPat) newVarType $ \env' -> do
          let scope' = F.extendScopePattern newVarPat scope
          bodyConstraints <- check scope' env' body (F.sink currentType)
          implLetBodyConstraint <-
            buildImplicationFromType "Chk-Let" scope' newVarPat (F.sink newVarType) bodyConstraints

          pure $ CAnd [newVarConstraints, implLetBodyConstraint]

  {- [Chk-Syn]
  G |- e ==> s        G |- s <: t
  ----------------------------------[Chk-Syn]
            G |- e <== t
  -}
  term -> withRule "[Chk-Syn]" $ do
    (synthsConstraints, termType) <- synths scope env term
    debugPrintT "on term" >> debugPrint term
    debugPrintT "calling subtype"
    debugPrintT "left type: " >> debugPrint termType
    debugPrintT "right type: " >> debugPrint currentType
    subtypingConstraints <- subtype scope termType currentType
    pure $ CAnd [synthsConstraints, subtypingConstraints]

buildImplicationFromType :: (F.DExt i o) => Text -> F.Scope o -> Pattern i o -> Term o -> Constraint -> CheckerM Constraint
buildImplicationFromType msg scope argVarPat@(PatternVar argVarId) typ constraint = case typ of
  TypeRefined base (PatternVar typVarId) p -> do
    let
      subst = F.addRename (F.sink F.identitySubst) typVarId (F.nameOf argVarId)
      p' = F.substitute scope subst p
      argVarIdRaw = getRawVarIdFromPattern argVarPat
      Inner.VarIdent argVarIdRawName = argVarIdRaw

    debugPrintT $ "Implication: " <> "varId = " <> showT argVarIdRawName <> ", term = " <> showT p'
    pure $ CImplication argVarIdRaw base (fromTerm p') constraint msg
  TypeFun{} -> pure constraint
  otherTerm -> throwError $
    "can't buildImplicationFromType\n"
    <> "context message: " <> msg <> "\n"
    <> "term: " <> showT otherTerm

synths ::
  (F.DExt F.VoidS i) =>
  F.Scope i ->
  Env i ->
  Term i ->
  CheckerM (Constraint, Term i)
synths scope env currentTerm = case currentTerm of
  {- [Syn-Var]
   -----------------
    G |- x ==> G(x)
  -}
  F.Var varId -> withRule "[Syn-Var]" $ do
    let typ = lookupEnv env varId
    debugPrintT $ "lookup: " <> showT currentTerm
    debugPrintT $ "typ: " <> showT typ
    pure (cTrue, typ)

  {- [Syn-Con]
   -----------------
    G |- x ==> G(x)
  -}
  ConstInt x -> withRule "[Syn-Con]" $ do
    debugPrintT "type: " >> debugPrint (constIntT x)
    pure (cTrue, F.sink $ constIntT x)

  {- [Syn-App]
   G |- e ==> x:s -> t       G |- y <== s
   --------------------------------------
   G |- e y ==> t[x := y]
  -}
  App funcTerm argTerm -> withRule "[Syn-App]" $ do
     -- G |- e ==> x:s -> t
    (funcConstraints, funcType) <- synths scope env funcTerm
    case funcType of
      TypeFun varPattern varType returnType -> do
        debugPrintT "checking argument"
        debugPrintT "argument term: " >> debugPrint argTerm
        debugPrintT "argument type: " >> debugPrint varType
        argConstraints <- check scope env argTerm varType
        let
          resultType =
            F.substitutePattern scope F.identitySubst varPattern [argTerm] returnType
        debugPrintT "resultType: " >> debugPrint resultType
        pure (CAnd [funcConstraints, argConstraints], resultType)
      _ -> error "unimplemented case"

  {- [Syn-Ann]
   G |- e <== t
   -----------------
   G |- e:t => t
  -}
  Ann annType term -> withRule "[Syn-Ann]" $ do
    checkConstraints <- check scope env term annType
    pure (checkConstraints, annType)

  {-
  [Syn-Con] (op type is predefined) + [Syn-App] (type operator like function application)
  -}
  OpExpr lTerm op rTerm -> withRule "OpExr [Syn-Con] + [Syn-App]" $ case binOpTypes scope op of
    BinOpType xBinder xType yBinder yType resType -> do
      leftTermConstraints <- check scope env lTerm xType
      let scopeWithX = F.extendScope xBinder scope
      -- чтоб проверить rTerm нам на самом деле не нужен xBinder в окружении, но нужен корректный env чтоб компилятор переварил.
      rightTermConstraints <- withExtendedEnv env xBinder xType $ \env' ->
        check scopeWithX env' (F.sink rTerm) yType
      let
        xSubst = F.addSubst F.identitySubst xBinder lTerm
        ySubst = F.addSubst F.identitySubst yBinder (F.sink rTerm)
        substitutedReturnType = F.substitute scope xSubst $
          F.substitute scopeWithX ySubst resType
      debugPrintT "return type" >> debugPrint substitutedReturnType
      pure (CAnd [leftTermConstraints, rightTermConstraints], substitutedReturnType)
  _ -> error "unimplemented case"

getNameBinderFromPattern :: Pattern i o -> F.NameBinder i o
getNameBinderFromPattern (PatternVar binder) = binder

getRawVarIdFromPattern :: Pattern i o -> Inner.VarIdent
getRawVarIdFromPattern varPat = case fromPattern varPat of
  Inner.PatternVar v -> v
