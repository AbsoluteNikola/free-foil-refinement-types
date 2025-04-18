{-# LANGUAGE TupleSections #-}
module Language.Sprite.TypeCheck.Types where

import Language.Sprite.Syntax
import Control.Monad.Foil qualified as F
import Control.Monad.Foil.Internal qualified as F
import Control.Monad.Free.Foil qualified as F
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import Language.Sprite.TypeCheck.Monad
import Control.Monad.Error.Class (MonadError(throwError))
import Data.Maybe (catMaybes)
import Language.Sprite.TypeCheck.Constraints (sortPred, getTypeSort)
import Data.Traversable (for)
import Data.Biapplicative (bimap)
import qualified Language.Fixpoint.Types.Sorts as FTS
import Data.Bifoldable (bifoldr)

constIntT :: Integer -> Term F.VoidS
constIntT x = F.withFreshBinder F.emptyScope $
  \binder -> TypeRefined BaseTypeInt (PatternVar binder) (OpExpr (F.Var (F.nameOf binder)) Inner.EqOp (ConstInt x))

anyIntT :: Term F.VoidS
anyIntT = F.withFreshBinder F.emptyScope $ \binder ->
    TypeRefined BaseTypeInt (PatternVar binder) (Boolean Inner.ConstTrue)

anyBoolT :: Term F.VoidS
anyBoolT = F.withFreshBinder F.emptyScope $ \binder ->
    TypeRefined BaseTypeBool (PatternVar binder) (Boolean Inner.ConstTrue)

boolWithT :: Inner.ConstBool -> Term F.VoidS
boolWithT b = F.withFreshBinder F.emptyScope $ \binder ->
    TypeRefined BaseTypeBool (PatternVar binder) (OpExpr (F.Var (F.nameOf binder)) Inner.EqOp (Boolean b))

extendTypeToCurrentScope :: F.Distinct i => F.Scope i -> Term i -> CheckerM (Term i)
extendTypeToCurrentScope scope typ = do
  case typ of
    TypeRefined b oldVar p -> F.withFreshBinder scope $ \newBinder ->
      case (F.assertDistinct newBinder, F.assertExt newBinder) of
        (F.Distinct, F.Ext) -> do
          let
            scope' = F.extendScope newBinder scope
            newPred = F.substitutePattern scope' (F.sink F.identitySubst) oldVar [F.Var (F.nameOf newBinder)] p
          pure $ TypeRefined b (PatternVar newBinder) newPred
    TypeData typName typArgs oldVar p ->  F.withFreshBinder scope $ \newBinder ->
      case (F.assertDistinct newBinder, F.assertExt newBinder) of
        (F.Distinct, F.Ext) -> do
          let
            scope' = F.extendScope newBinder scope
            newPred = F.substitutePattern scope' (F.sink F.identitySubst) oldVar [F.Var (F.nameOf newBinder)] p
          typArgs' <- for typArgs (extendTypeToCurrentScope scope)
          pure $ TypeData typName typArgs' (PatternVar newBinder) newPred
    TypeFun argName argTyp retTyp ->  F.withFreshBinder scope $ \newBinder ->
      case (F.assertDistinct newBinder, F.assertExt newBinder) of
        (F.Distinct, F.Ext) -> do
          argTypExtended <- extendTypeToCurrentScope scope argTyp
          let
            scope' = F.extendScope newBinder scope
            newRetType = F.substitutePattern scope' (F.sink F.identitySubst) argName [F.Var (F.nameOf newBinder)] retTyp
          newRetTypeExtended <- extendTypeToCurrentScope scope' newRetType
          pure $ TypeFun (PatternVar newBinder) argTypExtended newRetTypeExtended
    TypeForall v typUnderForall -> F.withFreshBinder scope $ \newBinder ->
      case (F.assertDistinct newBinder, F.assertExt newBinder) of
        (F.Distinct, F.Ext) -> do
          let
            scope' = F.extendScope newBinder scope
            newTypUnderForall = F.substitutePattern scope' (F.sink F.identitySubst) v [F.Var (F.nameOf newBinder)] typUnderForall
          newTypUnderForall' <- extendTypeToCurrentScope scope' newTypUnderForall
          pure $ TypeForall (PatternVar newBinder) newTypUnderForall'
    _ -> throwError $
      "extendTypeToCurrentScope should be called only on type, not term\n"
      <> showT typ

mkPredicatesInTypeUnknown :: F.Distinct i => F.Scope i -> Term i -> CheckerM (Term i)
mkPredicatesInTypeUnknown scope typ = do
  case typ of
    TypeRefined b v _ -> pure $ TypeRefined b v Unknown
    TypeData typName args v _ -> pure $ TypeData typName args v Unknown
    TypeFun argName argTyp retTyp -> case (F.assertDistinct argName, F.assertExt argName) of
        (F.Distinct, F.Ext) -> do
          argTypExtended <- mkPredicatesInTypeUnknown scope argTyp
          let
            scope' = F.extendScopePattern argName scope
          newRetTypeExtended <- mkPredicatesInTypeUnknown scope' retTyp
          pure $ TypeFun argName argTypExtended newRetTypeExtended
    TypeForall v typUnderForall ->
      case (F.assertDistinct v, F.assertExt v) of
        (F.Distinct, F.Ext) -> do
          let
            scope' = F.extendScopePattern v scope
          newTypUnderForall' <- mkPredicatesInTypeUnknown scope' typUnderForall
          pure $ TypeForall v newTypUnderForall'
    _ -> throwError $
      "mkPredicatesInTypeUnknown should be called only on type, not term\n"
      <> showT typ


{- |  see 4.3.2 Synthesis and figure 4.3

G(x) = b[v|p] -> b[v|p && v = x]
-}
singletonT ::
  (F.Distinct i) =>
  -- | var name
  F.Name i ->
  -- | var type
  Term i ->
  --  | type with singleton Type Strengthening
  Term i
singletonT varName typ = case typ of
  TypeRefined base (PatternVar typVar) predicate ->
    case (F.assertDistinct typVar, F.assertExt typVar) of
      (F.Distinct, F.Ext) -> TypeRefined base (PatternVar typVar)
        (OpExpr predicate Inner.AndOp
          (OpExpr (F.Var (F.sink varName)) Inner.EqOp (F.Var (F.nameOf typVar))))
  TypeData typeName typeArgs (PatternVar typVar) predicate ->
    case (F.assertDistinct typVar, F.assertExt typVar) of
      (F.Distinct, F.Ext) -> TypeData typeName typeArgs (PatternVar typVar)
        (OpExpr predicate Inner.AndOp
          (OpExpr (F.Var (F.sink varName)) Inner.EqOp (F.Var (F.nameOf typVar))))
  _ -> typ

{- See 5.4, Figure 5.4, page 34 -}
fresh ::F.Distinct i => F.Scope i -> Env i -> Term i -> CheckerM (Term i)
fresh scope env curType = case curType of
  {-
  fresh(G, b[?]) = b[v| k(v,...x) ]
    k = fresh horn variable of sorts b : ...x sorts
    v = fresh binder
    ...x = arguments from env
  -}
  TypeRefined base pat@(PatternVar patVar) refPred -> do
    namesAndSorts <- envSorts scope env
    typeSort <-  case getTypeSort curType of
      Right sort -> pure sort
      Left err -> throwError err
    case (F.assertDistinct pat, F.assertExt pat) of
      (F.Distinct, F.Ext) -> do
        refPred' <- case refPred of
          Unknown -> mkHornVarPred patVar typeSort namesAndSorts
          _ -> pure refPred
        pure $
          TypeRefined
            base
            (PatternVar patVar) -- v
            refPred'

  TypeData typName typArgs pat@(PatternVar patVar) refPred -> do
    namesAndSorts <- envSorts scope env
    typeSort <-  case getTypeSort curType of
      Right sort -> pure sort
      Left err -> throwError err
    typArgs' <- for typArgs (fresh scope env)
    case (F.assertDistinct pat, F.assertExt pat) of
      (F.Distinct, F.Ext) -> do
        refPred' <- case refPred of
          Unknown -> mkHornVarPred patVar typeSort namesAndSorts
          _ -> pure refPred
        pure $
          TypeData
            typName
            typArgs'
            (PatternVar patVar) -- v
            refPred'

  {-
  fresh(G, x:s -> t) = x:s' -> t'
    s' = fresh(G, s)
    t' = fresh(G; x:s, t)
  -}
  TypeFun (PatternVar argBinder) artTyp retTyp -> do
    argType' <- fresh scope env artTyp
    retType' <- case (F.assertDistinct argBinder, F.assertExt argBinder) of
      (F.Distinct, F.Ext) ->
        withExtendedEnv env argBinder artTyp $ \env' -> do
          let scope' = F.extendScope argBinder scope
          fresh scope' env' retTyp
    pure $ TypeFun (PatternVar argBinder) argType' retType'
  {-
  fresh(G, Forall x s) = Forall x s'
    s' = fresh(G, s)
  -}
  TypeForall (PatternVar typVarBinder) typeUnderForAll -> do
    typeUnderForAll' <- case (F.assertDistinct typVarBinder, F.assertExt typVarBinder) of
      (F.Distinct, F.Ext) -> do
        -- TODO: env problem here
        withExtendedEnv env typVarBinder Unknown $ \env' -> do
          let scope' = F.extendScope typVarBinder scope
          fresh scope' env' typeUnderForAll
    pure $ TypeForall (PatternVar typVarBinder) typeUnderForAll'

  otherTerm -> throwError $
    "fresh should be called only on type, not term:\n" <> showT otherTerm

mkHornVarPred ::
  ( F.DExt i o) =>
  F.NameBinder i o ->
  FTS.Sort ->
  [(F.Name i, FTS.Sort)] ->
  CheckerM (Term o)
mkHornVarPred freshBinder typeSort (unzip -> (names, sorts)) = do
  newHornVarName <- mkFreshHornVar (typeSort : sorts)
  debugPrintT $ "Horn var name: " <> showT newHornVarName
  debugPrintT $ "Horn var args: " <> showT names
  debugPrintT $ "Horn var sorts: " <> showT sorts
  let
    hornVarPred = HVar newHornVarName
      $ F.Var (F.nameOf freshBinder) : (F.Var . F.sink <$> names )
  pure hornVarPred

envSorts :: F.Distinct i => F.Scope i -> Env i -> CheckerM [(F.Name i, FTS.Sort)]
envSorts scope env = do
  namesAndSorts <- for (envToList env) $ \(name, typ) -> do
    sortPred scope name typ >>= \case
        Nothing -> pure Nothing
        Just (sort, _) -> pure $ Just (name, sort)
  pure $ catMaybes namesAndSorts

containsVar ::
  F.Distinct i =>
  F.Name i ->
  Term i ->
  Bool
containsVar neededVarName inType = f inType False
  where
    f term res = case term of
      F.Var name -> res || name == neededVarName
      F.Node node -> res || bifoldr fScoped f False node
    fScoped (F.ScopedAST binder body) res =
      case (F.assertDistinct binder, F.assertExt binder) of
        (F.Distinct, F.Ext) -> res || containsVar (F.sink neededVarName) body


-- | Тоже самое что и Foil.substitute, только правильно подставляет type var (она вложена в base typ)
-- Поэтому в переданная подстановка должна содержать например [a -> int[v|true]] и тогда на выходе будет 'a[v|v < 0] -> int[v|true]
-- TODO: сделать без костыля с RawName
substTypeVar ::
  F.Distinct i =>
  F.Scope i ->
  F.Substitution Term o i  ->
  F.RawName ->
  -- In what type
  Term o ->
  Term i
substTypeVar scope subst neededVar inType = case inType of
  TypeRefined (BaseTypeVar (F.Var v@(F.UnsafeName rawVar))) _ _
    | neededVar == rawVar
    ->
      let
        res = F.lookupSubst subst v
        -- msg = "1: Was" <> show inType <> "\nBecame " <> show res <> "\n"
      in res
  F.Var v ->
    let
      res = F.lookupSubst subst v
      -- msg = "2: Was " <> show inType <> "\nBecame " <> show res <> "\n"
    in res
  F.Node node ->
    let
      res = F.Node (bimap f (substTypeVar scope subst neededVar) node)
      -- msg = "3: Was " <> show inType <> "\nBecame " <> show res <> "\n"
    in res
  where
    f (F.ScopedAST binder body) =
      F.withRefreshedPattern scope binder $ \extendSubst binder' ->
        let subst' = extendSubst (F.sink subst)
            scope' = F.extendScopePattern binder' scope
            body' =  substTypeVar scope' subst' neededVar body
        in F.ScopedAST binder' body'

substTempTypeVar ::
  F.Distinct i =>
  F.Scope i ->
  -- | What temp type var name
  Inner.VarIdent ->
  -- | On what type change temp var
  Term i ->
  -- | should be identity subst
  F.Substitution Term o i  ->
  -- In what type
  Term o ->
  Term i
substTempTypeVar scope tempTypeVar typToSubst subst inType = case inType of
  TypeRefined (BaseTypeTempVar varId) _ _
    | tempTypeVar == varId -> typToSubst
  F.Var v -> F.lookupSubst subst v
  F.Node node -> F.Node (bimap f (substTempTypeVar scope tempTypeVar typToSubst subst) node)
  where
    f (F.ScopedAST binder body) =
        F.withRefreshedPattern scope binder $ \extendSubst binder' ->
          let subst' = extendSubst (F.sink subst)
              scope' = F.extendScopePattern binder' scope
              body' =  substTempTypeVar scope' tempTypeVar (F.sink typToSubst) subst' body
          in F.ScopedAST binder' body'

baseTypeEq :: Term i -> Term i -> Bool
baseTypeEq BaseTypeBool BaseTypeBool = True
baseTypeEq BaseTypeInt BaseTypeInt = True
baseTypeEq (BaseTypeVar (F.Var v1)) (BaseTypeVar (F.Var v2)) = v1 == v2
baseTypeEq (BaseTypeTempVar v1) (BaseTypeTempVar v2) = v1 == v2
baseTypeEq _ _ = False

-- see figure 7.7
unapply :: F.Distinct i => F.Scope i -> Env i -> Pattern i o -> Term i -> CheckerM (Env o, Term o)
unapply _ _ pat@(PatternVar'{}) _ = throwError
  $ "Invalid pattern for unapply: " <> showT pat
unapply _ env PatternNoBinders typ = pure (env, typ) -- TODO: add meet
unapply scope env
  (PatternSomeBinders newBinder otherPattern)
  (TypeFun funArgId argTyp retType) = case (F.assertDistinct newBinder, F.assertExt newBinder) of
    (F.Distinct, F.Ext) -> do
      let
        scope' = F.extendScopePattern newBinder scope
        retTypeSubst =
          F.addRename
            (F.sink F.identitySubst)
            (getNameBinderFromPattern funArgId)
            (F.nameOf newBinder)
        retTypeSubstituted = F.substitute scope' retTypeSubst retType
      withExtendedEnv env newBinder argTyp $ \env' ->
        unapply scope' env' otherPattern retTypeSubstituted

unapply _ _ pat typ = throwError
  $ "Can't unapply pattern " <> showT pat
  <> " with type " <> showT typ

ctor :: F.Distinct i => F.Scope i -> Term i -> Term i -> CheckerM (Term i)
ctor scope switchVarType conType = do
  switchVarConTypArgs <- case switchVarType of
    TypeData _ typArgs _ _ -> pure typArgs
    _ -> throwError $ "Constructor type not TypeData: " <> showT conType
  -- constructor function
  debugPrintT $ "switchVarConTypArgs: " <> showT switchVarConTypArgs
  debugPrintT $ "conType: " <> showT conType
  ctorGo scope switchVarConTypArgs conType

ctorGo :: F.Distinct i => F.Scope i -> [Term i] -> Term i -> CheckerM (Term i)
ctorGo _ [] t = pure t
-- ctorGo _ [] t = throwError $ "ctorGo, result type not TypeData: " <> showT t
ctorGo scope (typArg:typeArgs) conType = case conType of
  TypeForall (PatternVar typVarBinder) typeUnderForAll -> do
    let
      typeUnderForAllSubst = F.addSubst F.identitySubst typVarBinder typArg
      F.UnsafeName rawTypVarBinder = F.nameOf typVarBinder
      typeUnderForAllSubstituted = substTypeVar scope typeUnderForAllSubst rawTypVarBinder typeUnderForAll
    ctorGo scope typeArgs typeUnderForAllSubstituted
  t -> throwError $ "ctorGo, can't apply type argument to: " <> showT t

meet :: (F.Distinct o) => F.Scope o -> Term o -> Term o -> CheckerM (Term o)
meet scope (TypeRefined baseL varPatL refL) (TypeRefined baseR varPatR refR)
  | baseTypeEq baseL baseR =
    case (F.assertDistinct varPatL, F.assertExt varPatL) of
      (F.Distinct, F.Ext) -> do
        let
          scope' = F.extendScopePattern varPatL scope
          refSubst =
              F.addRename
                (F.sink F.identitySubst)
                (getNameBinderFromPattern varPatR)
                (F.nameOf $ getNameBinderFromPattern varPatL)
          refRSubstituted = F.substitute scope' refSubst refR
        pure $ TypeRefined baseL varPatL
          (OpExpr refL Inner.AndOp refRSubstituted)

meet scope
  (TypeData typeNameL typeArgsL varPatL refL)
  (TypeData typeNameR _ varPatR refR)
  | typeNameL == typeNameR =
    case (F.assertDistinct varPatL, F.assertExt varPatL) of
      (F.Distinct, F.Ext) -> do
        let
          scope' = F.extendScopePattern varPatL scope
          refSubst =
              F.addRename
                (F.sink F.identitySubst)
                (getNameBinderFromPattern varPatR)
                (F.nameOf $ getNameBinderFromPattern varPatL)
          refRSubstituted = F.substitute scope' refSubst refR
        pure $ TypeData typeNameL typeArgsL varPatL
          (OpExpr refL Inner.AndOp refRSubstituted)

meet _ t1 t2 = throwError $ "Can't meet\n"
  <> "left type: " <> showT t1
  <> "\nright type: " <> showT t2
