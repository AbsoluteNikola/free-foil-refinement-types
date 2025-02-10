{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Sprite.TypeCheck.Check where
import Control.Monad.Foil qualified as F
import Control.Monad.Foil.Internal qualified as F
import Control.Monad.Free.Foil qualified as F
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
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import Data.Biapplicative (Bifunctor)
import qualified Control.Monad.Foil.Relative as F
import Language.Sprite.TypeCheck.Predicates

data Env n where
    EmptyEnv :: Env F.VoidS
    NonEmptyEnv :: F.DExt i o => Env i -> F.NameBinder i o -> Term i -> Env o

newtype CheckerM a = CheckerM {runCheckerM :: (ExceptT Text IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadError Text, MonadIO)

withExtendedEnv :: (F.DExt i o) => Env i -> F.NameBinder i o -> Term i -> (Env o -> CheckerM a) -> CheckerM a
withExtendedEnv env binder typ action = do
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
debugPrint = pPrint

debugPrintT :: Text -> CheckerM ()
debugPrintT = liftIO . TIO.putStrLn

subtype :: F.Distinct i => F.Scope i -> Term  i -> Term i -> CheckerM Constraint

{- | [Sub-Base]
     (v::t) => q[w := v]
     -------------------
     b{v:p} <= b{w:q}
 -}
subtype scope (TypeRefined lb leftVarPat leftPredicate) (TypeRefined rb rightVarPat rightPredicate)
  | lb /= rb = throwError
    $ "Invalid subtyping. Different refined base: " <> pShowT lb <> " and " <> pShowT rb
  | otherwise = do
      let
        implMsg = "Refinement subtype error: (v::t) => q[w := v]. Where v="
          <> showT leftVarPat
          <> ", w=" <> showT rightVarPat
        predMsg = "Refinement subtype error: q[w := v]. Where w="
          <> showT rightVarPat <> " and v=" <> showT leftVarPat
      case unifyScopes scope (F.ScopedAST leftVarPat leftPredicate) (F.ScopedAST rightVarPat rightPredicate) of
        Nothing -> throwError "can't unify"
        Just (PairOfScopedAST binders' (leftPat, rightPat) _ rightPred) -> do
          let scope' = F.extendScopePattern binders' scope
          case  (F.assertExt binders', F.assertDistinct binders') of
            (F.Ext, F.Distinct) -> do
              let
                subst = F.addRename (F.sink F.identitySubst) (getNameBinderFromPatter rightPat) (F.nameOf $ getNameBinderFromPatter leftPat)
                predSubstituted = F.substitute scope' subst rightPred
                predConstraint = CPred (fromTerm predSubstituted) predMsg
              pure $ buildImplicationFromType implMsg scope' leftPat predSubstituted predConstraint

{- | [Sub-Fun]

    s2 <: s1    x2:s2 |- t1[x1:=x2] <: t2
    -------------------------------------
    x1:s1 -> t1 <: x2:s2 -> t2
-}
subtype scope
  (TypeFun leftFunArgPat leftFunArgT leftFunRetT)
  (TypeFun rightFunArgPat rightFunArgT rightFunRetT) = do
  --  s2 <: s1
  argSubtypingConstrains <- subtype scope rightFunArgT leftFunArgT
  case unifyScopes scope (F.ScopedAST leftFunArgPat leftFunRetT) (F.ScopedAST rightFunArgPat rightFunRetT) of
    Nothing -> throwError "can't unify"
    Just (PairOfScopedAST binders' (leftArgPat, rightArgPat) leftFunRetT' rightFunRetT') -> do
      let scope' = F.extendScopePattern binders' scope
      case  (F.assertExt binders', F.assertDistinct binders') of
        (F.Ext, F.Distinct) -> do
          let
            leftFunRetTSubst =
              F.addRename
                (F.sink F.identitySubst)
                (getNameBinderFromPatter leftArgPat)
                (F.nameOf (getNameBinderFromPatter rightArgPat))
            -- t1[x1:=x2]
            leftFunRetTypeSubstituted = F.substitute scope' leftFunRetTSubst leftFunRetT'
          -- x2:s2 |- t1[x1:=x2] <: t2
          returnTypesSubtypingConstraints <- subtype scope' leftFunRetTypeSubstituted rightFunRetT'
          let
            implMsg = "Function subtype error: x2:s2 |- t1[x1:=x2] <: t2. Where x2="  <> showT rightArgPat
            returnTypesConstraints = buildImplicationFromType implMsg scope' rightArgPat leftFunRetTypeSubstituted returnTypesSubtypingConstraints
          pure $ CAnd [argSubtypingConstrains, returnTypesConstraints]

subtype _ l r = throwError "can't subtype"

check :: (F.DExt F.VoidS i) =>
  F.Scope i ->
  Env i ->
  -- | term to check
  Term i ->
  -- | type to check with
  Term i ->
  CheckerM Constraint
check scope env currentTerm currentType = case currentTerm of
  {- [Chk-Lam]
  G, x:s |- e <== t
  --------------------------
  G |- \x.e <== x:s -> t
  -}
  Fun varPattern body -> case currentType of
    TypeFun typeFunArgIdPat argType returnType -> do
      case unifyScopes scope (F.ScopedAST varPattern body) (F.ScopedAST typeFunArgIdPat returnType) of
        Nothing -> error "can't unify"
        Just (PairOfScopedAST binders' (leftPat, _) body' returnType') -> do
          let
            scope' = F.extendScopePattern binders' scope
          case  (F.assertExt binders', F.assertDistinct binders') of
            (F.Ext, F.Distinct) -> do
              let bindersList = F.nameBindersList binders'
              bodyCheckConstraints :: Constraint <- withExtendedEnv' env bindersList argType $ \env' ->
                check scope' env' body' returnType'
              pure $
                buildImplicationFromType "Chk-Lam" scope' leftPat returnType' bodyCheckConstraints
    _ -> throwError $ "Function type should be Function, not: " <> pShowT currentType

  {- [Chk-Let]
    G |- e1 ==> t1        G, x:t1 |- e2 <== t2
    -------------------------------------------
        G |- let x = e1 in e2 <== t2
  -}
  Let newVarPat newVarValue body -> do
    -- G |- e1 ==> t1
    (newVarConstraints, newVarType) <- synths scope env newVarValue
    case (F.assertDistinct newVarPat, F.assertExt newVarPat) of
      (F.Distinct, F.Ext) ->  do
        --  x:t1 |- e2 <== t2
        withExtendedEnv env (getNameBinderFromPatter newVarPat) newVarType $ \env' -> do
          let scope' = F.extendScopePattern newVarPat scope
          bodyConstraints <- check scope' env' body (F.sink newVarType)
          let
            implLetBodyConstraint =
              buildImplicationFromType "Chk-Let" scope' newVarPat (F.sink newVarType) bodyConstraints

          pure $ CAnd [newVarConstraints, implLetBodyConstraint]

  {- [Chk-Syn]
  G |- e ==> s        G |- s <: t
  ----------------------------------[Chk-Syn]
            G |- e <== t
  -}
  term -> do
    (synthsConstraints, termType) <- synths scope env term
    subtypingConstraints <- subtype scope termType currentType
    pure $ CAnd [synthsConstraints, subtypingConstraints]

buildImplicationFromType :: (F.DExt i o) => Text -> F.Scope o -> Pattern i o -> Term o -> Constraint -> Constraint
buildImplicationFromType msg scope argVarPat@(PatternVar argVarId) typ constraint = case typ of
  TypeRefined base (PatternVar typVarId) p ->
    let
      subst = F.addRename (F.sink F.identitySubst) typVarId (F.nameOf argVarId)
      p' = F.substitute scope subst p
      argVarIdRaw = case fromPattern argVarPat of
        Inner.PatternVar v -> v
    in
      CImplication argVarIdRaw base (fromTerm p') constraint msg
  TypeFun{} -> constraint
  otherTerm -> error $ "can't buildImplicationFromType from: " <> show otherTerm

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
  F.Var varId -> do
    let typ = lookupEnv env varId
    pure (cTrue, typ)

  {- [Syn-Con]
   -----------------
    G |- x ==> G(x)
  -}
  ConstInt x -> do
    pure (cTrue, F.sink $ constIntT x)

  {- [Syn-App]
   G |- e ==> x:s -> t       G |- y <== s
   --------------------------------------
   G |- e y ==> t[x := y]
  -}
  App funcTerm argTerm -> do
     -- G |- e ==> x:s -> t
    (funcConstraints, funcType) <- synths scope env funcTerm
    case funcType of
      TypeFun varPattern varType returnType -> do
        argConstraints <- check scope env argTerm varType
        let
          resultType =
            F.substitutePattern scope F.identitySubst varPattern [argTerm] returnType
        pure (CAnd [funcConstraints, argConstraints], resultType)
      _ -> error "unimplemented case"

  Ann annType term -> do
    checkConstraints <- check scope env term annType
    pure (checkConstraints, annType)

  {-
  [Syn-Con] (op type is predefined) + [Syn-App] (type operator like function application)
  -}
  OpExpr lTerm op rTerm -> case binOpTypes scope op of
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
      pure (CAnd [leftTermConstraints, rightTermConstraints], substitutedReturnType)
  _ -> error "unimplemented case"

getNameBinderFromPatter :: Pattern i o -> F.NameBinder i o
getNameBinderFromPatter (PatternVar binder) = binder

-- assume than for now we have only PatternVar with one variable
-- (F.NameBinder l n, F.NameBinder l n) is lift and right binders after renaming
data PairOfScopedAST binder sig n where
  PairOfScopedAST :: F.NameBinders n l -> (Pattern n l, Pattern n l) -> F.AST Pattern sig l -> F.AST Pattern sig l -> PairOfScopedAST Pattern sig n

unifyScopes
  :: (F.Distinct n, F.UnifiablePattern Pattern, Bifunctor sig)
  => F.Scope n
  -> F.ScopedAST Pattern sig n
  -> F.ScopedAST Pattern sig n
  -> Maybe (PairOfScopedAST Pattern sig n)
unifyScopes scope (F.ScopedAST patternL@(PatternVar binderL) bodyL) (F.ScopedAST patternR@(PatternVar binderR) bodyR) =
  case F.unifyPatterns patternL patternR of
    F.SameNameBinders binders -> do
      Just (PairOfScopedAST binders (patternL, patternR) bodyL bodyR)

    F.RenameLeftNameBinder binders renameL -> do
      case (F.assertDistinct binders, F.assertExt binders) of
        (F.Distinct, F.Ext) -> do
          let newScope = F.extendScopePattern binders scope
              bodyL' = F.liftRM newScope (F.fromNameBinderRenaming renameL) bodyL
          Just (PairOfScopedAST binders (PatternVar $ renameL binderL, patternR) bodyL' bodyR)

    F.RenameRightNameBinder binders renameR -> do
      case (F.assertDistinct binders, F.assertExt binders) of
        (F.Distinct, F.Ext) -> do
          let newScope = F.extendScopePattern binders scope
              bodyR' = F.liftRM newScope (F.fromNameBinderRenaming renameR) bodyR
          Just (PairOfScopedAST binders (patternL, PatternVar $ renameR binderR) bodyL bodyR')

    F.RenameBothBinders binders renameL renameR -> do
      case (F.assertDistinct binders, F.assertExt binders) of
        (F.Distinct, F.Ext) -> do
          let newScope = F.extendScopePattern binders scope
              bodyL' = F.liftRM newScope (F.fromNameBinderRenaming renameL) bodyL
              bodyR' = F.liftRM newScope (F.fromNameBinderRenaming renameR) bodyR
          Just (PairOfScopedAST binders (PatternVar $ renameL binderL, PatternVar $ renameR binderR) bodyL' bodyR')

    F.NotUnifiable -> Nothing
