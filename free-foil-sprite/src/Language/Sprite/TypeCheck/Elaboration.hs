module Language.Sprite.TypeCheck.Elaboration (check) where

import Control.Monad.Foil qualified as F
import Control.Monad.Free.Foil qualified as F
import Language.Sprite.Syntax
import Control.Monad.Except (MonadError (throwError))
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import Language.Sprite.TypeCheck.Predicates
import Language.Sprite.TypeCheck.Types
import Language.Sprite.TypeCheck.Monad
import Data.Biapplicative (bimap)
import Unsafe.Coerce (unsafeCoerce)
import Debug.Trace (trace)

{-
С текущим алгоритмом унификации есть проблема, в том она унифицирует переменную только в терме
который был передан в unify. Если переменная унифицируется в другой ветке от TApp, то
в TApp останется голая temp var, что некорректно. В текущей реализации это является проблемой
при использовании каррирования
-}
unify ::
  F.Distinct i =>
  F.Scope i ->
  -- | left type to unify
  Term i ->
  -- | right type to unify
  Term i ->
  -- | term where to substitute tempTypeVariable on new type
  Term i ->
  -- | (term with substituted temp type variables, result type )
  CheckerM (Term i , Term i)
unify scope (TypeRefined (BaseTypeTempVar varId) _ _) t term = do
  debugPrintT "Unify 1!"
  let substitutedTerm = substTempTypeVar scope varId t F.identitySubst term
  pure (substitutedTerm, t)
unify scope t (TypeRefined (BaseTypeTempVar varId) _ _) term = withRule "[Unify]" $ do
  debugPrintT $ "VarId: " <> showT varId
  debugPrintT $ "Change on: " <> showT t
  debugPrintT $ "Was: " <> showT term
  let substitutedTerm = substTempTypeVar scope varId t F.identitySubst term
  debugPrintT $ "Became: " <> showT substitutedTerm
  pure (substitutedTerm, t)
unify _scope t1@(TypeRefined b1 _ _) _t2@(TypeRefined b2 _ _) term
  | baseTypeEq b1 b2 = pure (term, t1)
unify _scope t1@(TypeRefinedUnknown b1) _t2@(TypeRefinedUnknown b2) term
  | baseTypeEq b1 b2 = pure (term, t1)
unify _scope t1@(TypeRefined b1 _ _) _t2@(TypeRefinedUnknown b2) term
  | baseTypeEq b1 b2 = pure (term, t1)
unify _scope _t1@(TypeRefinedUnknown b1) t2@(TypeRefined b2 _ _) term
  | baseTypeEq b1 b2 = pure (term, t2)

unify scope (TypeFun v1 argTyp1 retTyp1) (TypeFun v2 argTyp2 retTyp2) term = do
  (term', argTyp) <- unify scope argTyp1 argTyp2 term
  case (F.assertDistinct v2, F.assertExt v2) of
    (F.Distinct, F.Ext) -> do
      let
        scope' = F.extendScopePattern v2 scope
        subst = F.addRename (F.sink F.identitySubst)
          (getNameBinderFromPattern v1)
          (F.nameOf $ getNameBinderFromPattern v2)
        retTyp1' =
          F.substitute scope' subst retTyp1
      (term'', retType) <- unify scope' retTyp1' retTyp2 (F.sink term')
      {- TODO: придумать как удалить unsafe coerce
      Из-за того что мы зашли под скоуп из TypeFun и синканули терм под него
      можно сказать что переменно v2 там нет, поэтому мы как бы можем вернуть терм
      обратно в его скоуп. Верно ли утверждение? Подумать?
      -}
      pure (unsafeCoerce term'', TypeFun v2 argTyp retType)

unify _ t1 t2 _ = throwError $
  "Can't unify:\n"
  <> "left type: " <> showT t1
  <> "\nright type: " <> showT t2


check :: (F.DExt F.VoidS i) =>
  F.Scope i ->
  Env i ->
  -- | term to check
  Term i ->
  -- | type to check with
  Term i ->
  CheckerM (Term i {- elaborated term -})
check scope env currentTerm currentType = case currentTerm of
  {- [Chk-Lam]
  G, x:s |- e[y := x] <== t
  --------------------------
  G |- \y.e <== x:s -> t
  -}
  Fun varPattern body -> withRule "[Chk-Lam]" $ do
    extendedCurrentType <- extendTypeToCurrentScope scope currentType
    case extendedCurrentType of
      -- На самом деле в процессе вывода делать переименования аргумента функции не обязательно,
      -- но retType и body имеют разные скоупы потому что расширяются разными паттернами
      -- в этом случае можно переименовать переменную в теле на переменную из типа,
      -- либо наоборот: в типе на переменную из аргумента. Сделал как в Check.hs
      TypeFun typeFunArgIdPat@(PatternVar typeFunArgIdBinder) argType returnType -> do
        case  (F.assertExt typeFunArgIdBinder, F.assertDistinct typeFunArgIdBinder) of
          (F.Ext, F.Distinct) -> do
            let
              scope' = F.extendScopePattern typeFunArgIdBinder scope
              bodySubst =
                F.addRename
                  (F.sink F.identitySubst)
                  (getNameBinderFromPattern varPattern)
                  (F.nameOf typeFunArgIdBinder)
              bodySubstituted = F.substitute scope' bodySubst body
            elaboratedBody <- withExtendedEnv env typeFunArgIdBinder argType $ \env' ->
              check scope' env' bodySubstituted returnType
            debugPrintT $ "Arg type: " <> showT argType
            -- В терме теперь новое имя потому что мы y переименовали в x
            pure $ Fun typeFunArgIdPat elaboratedBody
      _ -> throwError $ "Function type should be Function, not: " <> showT extendedCurrentType

  {- [Chk-Let]
    G |- e1 ==> t1        G, x:t1 |- e2 <== t2
    -------------------------------------------
        G |- let x = e1 in e2 <== t2
  -}
  Let newVarPat newVarValue body -> withRule "[Chk-Let]" $ do
    -- G |- e1 ==> t1
    debugPrintT $  "synths " <> showT newVarPat
    (newVar', newVarType) <- synths scope env newVarValue
    case (F.assertDistinct newVarPat, F.assertExt newVarPat) of
      (F.Distinct, F.Ext) ->  do
        --  x:t1 |- e2 <== t2
        withExtendedEnv env (getNameBinderFromPattern newVarPat) newVarType $ \env' -> do
          let scope' = F.extendScopePattern newVarPat scope
          extendedCurrentType <- extendTypeToCurrentScope scope' (F.sink currentType)
          elaboratedBody <- check scope' env' body extendedCurrentType
          pure $ Let newVarPat newVar' elaboratedBody

  {- [Chk-Rec]
    G |- t1 : k
    G, x:t1 |- e1 <== t1
    G, x:t1 |- e2 <== t2
    ----------------------------------------------
        G |- let rec x = e1 : s1 in e2 <== t2
  -}
  LetRec newVarType newVarPat1 newVarPat2 newVarValue body -> withRule "[Chk-Rec]" $ do
    debugPrintT $  "new var type: " <> showT newVarType
    extendedVarType <- extendTypeToCurrentScope scope newVarType
    debugPrintT $  "extended new var type: " <> showT extendedVarType
    -- G |- t1 : k, t1 - newVarType
    case (F.assertDistinct newVarPat1, F.assertDistinct newVarPat2, F.assertExt newVarPat1,  F.assertExt newVarPat2) of
      (F.Distinct, F.Distinct, F.Ext, F.Ext) ->  do
        --  G, x:t1 |- e1 <== t1
        debugPrintT $  "check new var: " <> showT newVarPat1
        newVarValue' <- withExtendedEnv env (getNameBinderFromPattern newVarPat1) extendedVarType $ \env' -> do
          let
            scope' = F.extendScopePattern newVarPat1 scope
          check scope' env' newVarValue (F.sink extendedVarType)
          -- G, x:t1 |- e2 <== t2
        body' <- withExtendedEnv env (getNameBinderFromPattern newVarPat2) extendedVarType $ \env' -> do
          let scope' = F.extendScopePattern newVarPat2 scope
          extendedCurrentType <- extendTypeToCurrentScope scope' (F.sink currentType)
          check scope' env' body extendedCurrentType
        pure $ LetRec newVarType newVarPat1 newVarPat2 newVarValue' body'

  {- [Chk-If]
  G |- e1 <== t     G |- e2 <== t
  -----------------------------------------------------------------------------
      G |- if x then e1 else e2 <== t
  -}
  If cond thenTerm elseTerm -> withRule "[Chk-If]" $ do
    extendedCurrentType <- extendTypeToCurrentScope scope currentType
    cond' <- check scope env cond (F.sink anyBoolT)
    thenTerm' <-  check scope env thenTerm extendedCurrentType
    elseTerm' <- check scope env elseTerm extendedCurrentType
    pure $ If cond' thenTerm' elseTerm'

  {- [Chk-Syn]
  G |- e ==> s        G |- s <: t
  ----------------------------------[Chk-Syn]
            G |- e <== t
  -}
  term -> withRule "[Chk-Syn]" $ do
    (term', termType) <- synths scope env term
    (resTerm, _resType)<- unify scope currentType termType term'
    pure resTerm

synths ::
  (F.DExt F.VoidS i) =>
  F.Scope i ->
  Env i ->
  Term i ->
  CheckerM (Term i {- term -}, Term i {- typ -})
synths scope env currentTerm = case currentTerm of
  {- [Syn-Var]
    G(x) = t
   ----------------------
    G |- x ==> self(x, t)
  -}
  F.Var varId -> withRule "[Syn-Var]" $ do
    typ <- extendTypeToCurrentScope scope $ lookupEnv env varId
    debugPrintT $ "Getting: " <> showT currentTerm
    debugPrintT $ "Type: " <> showT typ
    mkTAppIfNecessary scope currentTerm typ


  {- [Syn-Con]
   -----------------
    G |- x ==> G(x)
  -}
  ConstInt x -> withRule "[Syn-Con]" $ do
    debugPrintT "type: " >> debugPrint (constIntT x)
    (currentTerm, ) <$> extendTypeToCurrentScope scope (F.sink (constIntT x))

  {- [Syn-App]
   G |- e ==> x:s -> t       G |- y <== s
   --------------------------------------
   G |- e y ==> t[x := y]
  -}
  App funcTerm argTerm -> withRule "[Syn-App]" $ do
     -- G |- e ==> x:s -> t
    (funcTerm', funcType) <- synths scope env funcTerm
    case funcType of
      TypeFun varPattern varType returnType -> do
        (argTerm', argType) <- synths scope env argTerm
        (funcTerm'', _argType') <-  unify scope argType varType funcTerm'
        debugPrintT $ "Arg type: " <> showT argType
        debugPrintT $ "Var type: " <> showT varType
        debugPrintT $ "Before unification: " <> showT funcTerm'
        debugPrintT $ "After unification: " <> showT funcTerm''
        let
          -- Сделать только для того чтоб сравнять скоупы
          resultType =
            F.substitutePattern scope F.identitySubst varPattern [argTerm] returnType
        -- Из-за алгоритма унифакации надо отдельно делать это для return type, чтоб подставить туда TempVar
        -- В идеале сделать список всех подстановок, но я пока не придумал как это сделать
        (resultType', _) <-  unify scope argType varType resultType
        debugPrintT $ "Return type: " <> showT returnType
        debugPrintT $ "Result type: " <> showT resultType
        pure (App funcTerm'' argTerm', resultType')
      _ -> throwError $ "Application to non function: " <> showT funcType

  {-
  [Syn-Con] (op type is predefined) + [Syn-App] (type operator like function application)
  -}
  OpExpr lTerm op rTerm -> withRule "OpExr [Syn-Con] + [Syn-App]" $ do
    let
      (argTypBase, retTypBase) = getBaseTypesForOp scope op
    argTyp <- extendTypeToCurrentScope scope =<< case argTypBase of
      BaseTypeInt -> pure $ F.sink anyIntT
      BaseTypeBool -> pure $ F.sink anyBoolT
      _ -> throwError $ "Unknown base type of operator args" <> showT argTypBase
    retTyp <- extendTypeToCurrentScope scope =<< case retTypBase of
      BaseTypeInt -> pure $ F.sink anyIntT
      BaseTypeBool -> pure $ F.sink anyBoolT
      _ -> throwError $ "Unknown base type for operator return type" <> showT argTypBase
    lTerm' <- check scope env lTerm argTyp
    rTerm' <- check scope env rTerm argTyp
    pure (OpExpr lTerm' op rTerm', retTyp)

  Boolean b -> do
    typ <- extendTypeToCurrentScope scope $ F.sink $ boolWithT b
    pure (currentTerm, typ)

  {- [Syn-Ann]
   G |- e <== t
   -----------------
   G |- e:t => t
  -}
  Ann annType term -> withRule "[Syn-Ann]" $ do
    extAnnType <- extendTypeToCurrentScope scope annType
    -- Проблема в том что мы хотим запустить check на типе без forall
    -- Но Forall накидывает скоупы, из-за этого мы не можем тип под forall поднять на верх
    -- решение - вызвать check term annType внутри mkTLamIfNecessary
    -- term' <- check scope env term extAnnType
    termWIthTLam <- mkTLamIfNecessary scope env term extAnnType
    pure (Ann annType termWIthTLam, extAnnType)

  _ -> throwError $ "unimplemented case: " <> showT currentTerm

getNameBinderFromPattern :: Pattern i o -> F.NameBinder i o
getNameBinderFromPattern (PatternVar binder) = binder

mkTAppIfNecessary ::
  (F.DExt F.VoidS i) =>
  F.Scope i ->
  -- | Term
  Term i ->
  -- | type
  Term i ->
  CheckerM (Term i {- new term with TApp -}, Term i {- new typ without Forall -})
mkTAppIfNecessary scope term typ = case typ of
  TypeForall (PatternVar typVarBinder) typUnderForall -> do
    case (F.assertDistinct typVarBinder, F.assertExt typVarBinder) of
      (F.Distinct, F.Ext) -> do
        (F.sink -> newTempTypVar) <- mkFreshTempTypVar
        let
          resultTypeSubst = F.addSubst F.identitySubst typVarBinder newTempTypVar
        resultType <- extendTypeToCurrentScope scope $
            substTypeVar scope resultTypeSubst typUnderForall
        debugPrintT $ "Was: " <> showT typUnderForall
        debugPrintT $ "Became: " <> showT resultType
        (term', typ') <- mkTAppIfNecessary scope (TApp term newTempTypVar) resultType
        typ'' <- extendTypeToCurrentScope scope typ'
        pure (term', typ'')
  -- все Forall у нас наверху типа, смотреть FrontToInner.hs mkForAll поэтому если мы встретили что-то иное то возвращать терм как есть
  _ -> do
    debugPrintT $ "mkTApp result term: " <> showT term
    debugPrintT $ "mkTApp result typ: " <> showT typ
    pure (term, typ)

mkTLamIfNecessary ::
  (F.DExt F.VoidS i) =>
  F.Scope i ->
  Env i ->
  -- | Term
  Term i ->
  -- | type
  Term i ->
  CheckerM (Term i {- new term with TApp -})
mkTLamIfNecessary scope env term typ = case typ of
  TypeForall typVar typUnderForall ->
    case (F.assertDistinct typVar, F.assertExt typVar) of
      (F.Distinct, F.Ext) -> do
        let
          scope' = F.extendScopePattern typVar scope
        -- на самом деле нам тут env расширять не нужно, но скоупы есть скоупы
        term' <- withExtendedEnv env (getNameBinderFromPattern typVar) (F.sink anyIntT) $ \env' ->
          mkTLamIfNecessary scope' env' (F.sink term) typUnderForall
        pure (TAbs typVar term')
  {-
  Есть некоторый нюанс в том что если мы спускаем term i вниз через sink i1 -> i2 -> i3.
  И в этих скоупах добавляются новые биндеры, то у изначально терма i со своими биндерами i1 -> i2 -> i3,
  может быть конфликт имен. Поэтому надо рефрешнуть все биндеру снизу у изначального терма

  Проблема что check вызывается здесь а не в Syn-Ann
  Forall накидывает скоупы, из-за этого мы не можем тип под forall поднять на верх
  решение - вызвать check term annType внутри mkTLamIfNecessary
  -}
  _ -> check scope env term typ

-- | Тоже самое что и Foil.substitute, только правильно подставляет type var (она вложена в base typ)
-- Поэтому в переданная подстановка должна содержать например [a -> int[v|true]] и тогда на выходе будет 'a[v|v < 0] -> int[v|true]
substTypeVar ::
  F.Distinct i =>
  F.Scope i ->
  F.Substitution Term o i  ->
  -- In what type
  Term o ->
  Term i
substTypeVar scope subst inType = case inType of
  TypeRefined (BaseTypeVar (F.Var v)) _ _
    -> F.lookupSubst subst v
  TypeRefinedUnknown (BaseTypeVar (F.Var v))
    -> F.lookupSubst subst v
  F.Var v -> F.lookupSubst subst v
  F.Node node -> F.Node (bimap f (substTypeVar scope subst) node)
  where
    f (F.ScopedAST binder body) =
      F.withRefreshedPattern scope binder $ \extendSubst binder' ->
        let subst' = extendSubst (F.sink subst)
            scope' = F.extendScopePattern binder' scope
            body' =  substTypeVar scope' subst' body
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
    | tempTypeVar == varId -> trace "substTempTypeVar matched!" $ typToSubst
  TypeRefinedUnknown (BaseTypeTempVar varId)
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
