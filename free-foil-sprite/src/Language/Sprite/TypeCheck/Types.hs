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
import Language.Sprite.TypeCheck.Constraints (baseTypeToSort)
import Data.Traversable (for)
import Data.Biapplicative (bimap)
import qualified Language.Fixpoint.Types as FTS
import Data.Text (Text)

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
  debugPrintT $ "Extending: " <> showT typ
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
  debugPrintT $ "Extending: " <> showT typ
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
  TypeRefined base _ Unknown -> do
    (catMaybes -> unzip -> (names, sorts)) <-
      for (envToList env) $ \(name, typ) -> case getBaseType typ of
        Nothing -> pure Nothing
        Just b -> case baseTypeToSort (fromTerm b) of
          Right sort -> pure $ Just (name, sort)
          Left err -> throwError err
    typeSort <-  case baseTypeToSort (fromTerm base) of
      Right sort -> pure sort
      Left err -> throwError err
    newHornVarName <- mkFreshHornVar (typeSort : sorts)
    debugPrintT $ "Horn var name: " <> showT newHornVarName
    debugPrintT $ "Horn var args: " <> showT names
    debugPrintT $ "Horn var sorts: " <> showT sorts
    F.withFreshBinder scope $ \freshBinder ->
      case (F.assertDistinct freshBinder, F.assertExt freshBinder) of
        (F.Distinct, F.Ext) -> pure $
          TypeRefined
            base
            (PatternVar freshBinder) -- v
            -- k(v,...x)
            (HVar newHornVarName $ F.Var (F.nameOf freshBinder) : (F.Var . F.sink <$> names ))


  {-
  fresh(G, b[v|p]) = b[v|p]
  -}
  t@TypeRefined{} -> pure t

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


getTypeSort :: Term i -> Either Text FTS.Sort
getTypeSort = \case
  TypeRefined b _ _ -> baseTypeToSort (fromTerm b)
  TypeData (Inner.VarIdent typName) typArgs _ _ -> do
    argsSorts <- traverse getTypeSort typArgs
    pure $ FTS.fAppTC
      (FTS.symbolFTycon . FTS.dummyLoc  . FTS.symbol $ typName)
      argsSorts
  TypeFun _ argTyp retTyp -> do
    argSort <- getTypeSort argTyp
    retSort <- getTypeSort retTyp
    pure $ FTS.FFunc argSort retSort
  TypeForall typVarPat typeUnderForAll -> do
    typeUnderForAllSort <- getTypeSort typeUnderForAll
    typVarRawId <- case fromPattern typVarPat of
      (Inner.PatternVar (Inner.VarIdent typVarRawId)) -> Right $ FTS.symbol typVarRawId
      pat -> Left $ "Unknown pattern in forall: " <> showT pat
    let
      -- Тут используется хак, что внутреннее представление Foil это Int
      -- и каждый биндер уникальный. Поэтому мы его и используется в fixpoint sort
      typVarIndex = F.nameId . F.nameOf $ getNameBinderFromPattern typVarPat
      subst = FTS.mkSortSubst [(typVarRawId, FTS.FVar typVarIndex)]
    pure $ FTS.FAbs typVarIndex (FTS.sortSubst subst typeUnderForAllSort)
  term -> Left $ "getTypeSort called on: " <> showT term

getBaseType :: Term i -> Maybe (Term i)
getBaseType = \case
  TypeRefined base _ _ -> Just base
  _ -> Nothing

baseTypeEq :: Term i -> Term i -> Bool
baseTypeEq BaseTypeBool BaseTypeBool = True
baseTypeEq BaseTypeInt BaseTypeInt = True
baseTypeEq (BaseTypeVar (F.Var v1)) (BaseTypeVar (F.Var v2)) = v1 == v2
baseTypeEq (BaseTypeTempVar v1) (BaseTypeTempVar v2) = v1 == v2
baseTypeEq _ _ = False
