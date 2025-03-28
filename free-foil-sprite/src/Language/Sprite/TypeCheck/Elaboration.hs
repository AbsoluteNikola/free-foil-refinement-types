module Language.Sprite.TypeCheck.Elaboration (check) where

import Control.Monad.Foil qualified as F
import Control.Monad.Foil.Internal qualified as F
import Control.Monad.Free.Foil qualified as F
import Language.Sprite.Syntax
import Control.Monad.Except (MonadError (throwError))
import Language.Sprite.TypeCheck.Predicates
import Language.Sprite.TypeCheck.Types
import Language.Sprite.TypeCheck.Monad
import Control.Monad (foldM)
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import Data.Maybe (mapMaybe)
import Unsafe.Coerce (unsafeCoerce)

type TempTypeVarUnification i = [(Inner.VarIdent, Term i)]

{-
В процессе вывода типов имеют значение только BaseTypes, предикаты будут проверяться и выводиться в
процессе проверки уточняющих типов. Все предикаты типов в TApp становятся Unknown.

алгоритм унификации (TempTypeVarUnification i) это результат унификации из BaseTypeTempVar -> Term i
с учетом того что все предикаты в подставляемых типах становятся Unknown получается что функциональные типы теперь независимые.
но привести все типы к скоупу Void мы тоже не можем, потому что есть зависимость на TypeVar из TypeFun.
Из-за этого введена специальная функция для отброса всех результатов унификации которые используют переменную расширяущую скоуп.
Смотреть unsinkTypeVarsMapping.

TempTypeVarUnification
-}
unify ::
  F.Distinct i =>
  F.Scope i ->
  -- | left type to unify
  Term i ->
  -- | right type to unify
  Term i ->
  -- | (term with substituted temp type variables, result type, type vars mapping)
  CheckerM (Term i, TempTypeVarUnification i)
unify scope (TypeRefined (BaseTypeTempVar varId) _ _) t = do
  t' <- mkPredicatesInTypeUnknown scope t
  debugPrintT $ "unify l VarId: " <> showT varId
  debugPrintT $ "unify l  Change on: " <> showT t'
  pure (t', [(varId, t')])


unify scope t (TypeRefined (BaseTypeTempVar varId) _ _) = withRule "[Unify]" $ do
  t' <- mkPredicatesInTypeUnknown scope t
  debugPrintT $ "unify r VarId: " <> showT varId
  debugPrintT $ "unify r Change on: " <> showT t'
  pure (t', [(varId, t')])

unify _scope t1@(TypeRefined b1 _ _) _t2@(TypeRefined b2 _ _)
  | baseTypeEq b1 b2 = pure (t1, [])

unify scope (TypeFun v1 argTyp1 retTyp1) (TypeFun v2 argTyp2 retTyp2) = do
  (argTyp, typeVarsMappingArgs) <- unify scope argTyp1 argTyp2
  case (F.assertDistinct v2, F.assertExt v2) of
    (F.Distinct, F.Ext) -> do
      let
        scope' = F.extendScopePattern v2 scope
        subst = F.addRename (F.sink F.identitySubst)
          (getNameBinderFromPattern v1)
          (F.nameOf $ getNameBinderFromPattern v2)
        retTyp1' =
          F.substitute scope' subst retTyp1
      (retType, typeVarsMappingRets) <- unify scope' retTyp1' retTyp2
      let
        typeVarsMapping
          = typeVarsMappingArgs ++ unsinkTypeVarsMapping (getNameBinderFromPattern v2) typeVarsMappingRets
      pure (TypeFun v2 argTyp retType, typeVarsMapping)

unify scope
  (TypeData lTypName lTypArgs lPredVar lPred)
  (TypeData rTypName rTypArgs _ _)
  | lTypName == rTypName = do
  let
    go (args, typeVarsMapping) (lArg, rArg) = do
      (arg', typeVarsMapping') <- unify scope lArg rArg
      pure (args ++ [arg'], typeVarsMapping ++ typeVarsMapping')
  (args', typeVarsMapping) <- foldM go ([], []) (zip lTypArgs rTypArgs)
  pure (TypeData lTypName args' lPredVar lPred, typeVarsMapping)

unify _ t1 t2 = throwError $
  "Can't unify:\n"
  <> "left type: " <> showT t1
  <> "\nright type: " <> showT t2

unsinkTypeVarsMapping :: F.DExt i o => F.NameBinder i o -> [(Inner.VarIdent, Term o)] -> [(Inner.VarIdent, Term i)]
unsinkTypeVarsMapping typVar typVarsMapping = flip mapMaybe typVarsMapping $
  \(tempTypVarId, typ) ->
    if containsVar (F.nameOf typVar) typ
      then Nothing
      else Just (tempTypVarId, unsafeCoerce typ)

applyTypeVarsMapping :: F.Distinct i => F.Scope i -> Term i -> [(Inner.VarIdent, Term i)] -> Term i
applyTypeVarsMapping scope = foldr $
  \(typVarName, typ) term ->
    substTempTypeVar scope typVarName typ F.identitySubst term

check :: (F.DExt F.VoidS i) =>
  F.Scope i ->
  Env i ->
  -- | term to check
  Term i ->
  -- | type to check with
  Term i ->
  CheckerM (Term i {- elaborated term -}, TempTypeVarUnification i)
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
            (elaboratedBody, typeVarsMapping) <- withExtendedEnv env typeFunArgIdBinder argType $ \env' ->
              check scope' env' bodySubstituted returnType
            debugPrintT $ "Type fun arg type: " <> showT argType
            -- В терме теперь новое имя потому что мы y переименовали в x
            let
              finalTypeVarsMapping = unsinkTypeVarsMapping typeFunArgIdBinder typeVarsMapping
              elaboratedBody' =  applyTypeVarsMapping scope' elaboratedBody typeVarsMapping
            pure $ (Fun typeFunArgIdPat elaboratedBody', finalTypeVarsMapping)
      _ -> throwError $ "Function type should be Function, not: " <> showT extendedCurrentType

  {- [Chk-Let]
    G |- e1 ==> t1        G, x:t1 |- e2 <== t2
    -------------------------------------------
        G |- let x = e1 in e2 <== t2
  -}
  Let newVarPat newVarValue body -> withRule "[Chk-Let]" $ do
    -- G |- e1 ==> t1
    debugPrintT $  "synths " <> showT newVarPat
    (newVar', newVarType, typeVarsMapping1) <- synths scope env newVarValue
    case (F.assertDistinct newVarPat, F.assertExt newVarPat) of
      (F.Distinct, F.Ext) ->  do
        --  x:t1 |- e2 <== t2
        withExtendedEnv env (getNameBinderFromPattern newVarPat) newVarType $ \env' -> do
          let scope' = F.extendScopePattern newVarPat scope
          extendedCurrentType <- extendTypeToCurrentScope scope' (F.sink currentType)
          (elaboratedBody, typeVarsMapping2) <- check scope' env' body extendedCurrentType
          let
            elaboratedBody' = applyTypeVarsMapping scope' elaboratedBody typeVarsMapping2
            finalTypeVarsMapping
              = typeVarsMapping1
              ++ unsinkTypeVarsMapping (getNameBinderFromPattern newVarPat) typeVarsMapping2
            letTerm = applyTypeVarsMapping scope
              (Let newVarPat newVar' elaboratedBody')
              finalTypeVarsMapping
          pure (letTerm, finalTypeVarsMapping)

  {- [Chk-Rec]
    G |- t1 : k
    G, x:t1 |- e1 <== t1
    G, x:t1 |- e2 <== t2
    ----------------------------------------------
        G |- let rec x = e1 : s1 in e2 <== t2
  -}
  LetRec newVarType newVarPat1 newVarPat2 newVarValue body -> withRule "[Chk-Rec]" $ do
    extendedVarType <- extendTypeToCurrentScope scope newVarType
    debugPrintT $  "new var type: " <> showT newVarType
    debugPrintT $  "extended new var type: " <> showT extendedVarType
    -- G |- t1 : k, t1 - newVarType
    case (F.assertDistinct newVarPat1, F.assertDistinct newVarPat2, F.assertExt newVarPat1,  F.assertExt newVarPat2) of
      (F.Distinct, F.Distinct, F.Ext, F.Ext) ->  do
        --  G, x:t1 |- e1 <== t1
        debugPrintT $  "check new var: " <> showT newVarPat1
        (newVarValue', typeVarsMapping1)  <- withExtendedEnv env (getNameBinderFromPattern newVarPat1) extendedVarType $ \env' -> do
          let
            scope' = F.extendScopePattern newVarPat1 scope
          -- повторно расширили тип, потому что x появился в контексте и был конфликт при использовании типов
          -- Просто синк тут не подходил, потому что переменная из типа затирала x в env
          -- уже применили результат унификации внутри
          mkTLamIfNecessary scope' env' newVarValue
            =<< extendTypeToCurrentScope scope' (F.sink extendedVarType)
          -- check scope' env' newVarValue (F.sink extendedVarType)

        -- G, x:t1 |- e2 <== t2
        (body', typeVarsMapping2) <- withExtendedEnv env (getNameBinderFromPattern newVarPat2) extendedVarType $ \env' -> do
          let scope' = F.extendScopePattern newVarPat2 scope
          extendedCurrentType <- extendTypeToCurrentScope scope' (F.sink currentType)
          (body', typeVarsMapping2) <- check scope' env' body extendedCurrentType
          pure  (applyTypeVarsMapping scope' body' typeVarsMapping2, typeVarsMapping2)
        let
          finalTypeVarsMapping
            = unsinkTypeVarsMapping (getNameBinderFromPattern newVarPat1) typeVarsMapping1
            ++ unsinkTypeVarsMapping (getNameBinderFromPattern newVarPat2) typeVarsMapping2
          letRecTerm = applyTypeVarsMapping scope
            (LetRec newVarType newVarPat1 newVarPat2 newVarValue' body')
            finalTypeVarsMapping
        pure (letRecTerm, finalTypeVarsMapping)

  {- [Chk-If]
  G |- e1 <== t     G |- e2 <== t
  -----------------------------------------------------------------------------
      G |- if x then e1 else e2 <== t
  -}
  If cond thenTerm elseTerm -> withRule "[Chk-If]" $ do
    extendedCurrentType <- extendTypeToCurrentScope scope currentType
    (cond', typeVarsMapping1) <- check scope env cond (F.sink anyBoolT)
    (thenTerm', typeVarsMapping2) <-  check scope env thenTerm extendedCurrentType
    (elseTerm', typeVarsMapping3) <- check scope env elseTerm extendedCurrentType
    let
      finalTypeVarsMapping = typeVarsMapping1 ++ typeVarsMapping2 ++ typeVarsMapping3
      cond'' = applyTypeVarsMapping scope cond' finalTypeVarsMapping
      thenTerm'' = applyTypeVarsMapping scope thenTerm' finalTypeVarsMapping
      elseTerm'' = applyTypeVarsMapping scope elseTerm' finalTypeVarsMapping
    pure $ (If cond'' thenTerm'' elseTerm'', finalTypeVarsMapping)

  {- [Chk-Syn]
  G |- e ==> s        G |- s <: t
  ----------------------------------[Chk-Syn]
            G |- e <== t
  -}
  term -> withRule "[Chk-Syn]" $ do
    (term', termType, typeVarsMapping1) <- synths scope env term
    (_resType, typeVarsMapping2) <- unify scope currentType termType
    let
      finalTypeVarsMapping = typeVarsMapping1 ++ typeVarsMapping2
      resTerm' = applyTypeVarsMapping scope term' finalTypeVarsMapping
    pure (resTerm', finalTypeVarsMapping)

synths ::
  (F.DExt F.VoidS i) =>
  F.Scope i ->
  Env i ->
  Term i ->
  CheckerM (Term i {- term -}, Term i {- typ -}, TempTypeVarUnification i)
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
    (termWithTApp, resultType) <- mkTAppIfNecessary scope currentTerm typ
    pure (termWithTApp, resultType, [])

  Constructor conName ->  withRule "[Syn-Constructor]" $ do
    typ <- lookupConstructor conName >>= extendTypeToCurrentScope scope . F.sink
    (termWithTApp, resultType) <- mkTAppIfNecessary scope currentTerm typ
    pure (termWithTApp, resultType, [])

  {- [Syn-Con]
   -----------------
    G |- x ==> G(x)
  -}
  ConstInt x -> withRule "[Syn-Con]" $ do
    debugPrintT "type: " >> debugPrint (constIntT x)
    (currentTerm, , []) <$> extendTypeToCurrentScope scope (F.sink (constIntT x))

  {- [Syn-App]
   G |- e ==> x:s -> t       G |- y <== s
   --------------------------------------
   G |- e y ==> t[x := y]
  -}
  App funcTerm argTerm -> withRule "[Syn-App]" $ do
     -- G |- e ==> x:s -> t
    (funcTerm', funcType, typeVarsMapping1) <- synths scope env funcTerm
    debugPrintT $ "Arg term: " <> showT argTerm
    debugPrintT $ "Func term: " <> showT funcTerm
    case funcType of
      TypeFun varPattern varType returnType -> do
        (argTerm', argType, typeVarsMapping2) <- synths scope env argTerm
        (_argType', typeVarsMapping3) <-  unify scope argType varType
        debugPrintT $ "Arg type: " <> showT argType
        debugPrintT $ "Var type: " <> showT varType
        debugPrintT $ "Before unification: " <> showT funcTerm'
        -- debugPrintT $ "After unification: " <> showT funcTerm''
        let
          -- Сделать только для того чтоб сравнять скоупы
          resultType =
            F.substitutePattern scope F.identitySubst varPattern [argTerm] returnType
        -- Из-за алгоритма унифакации надо отдельно делать это для return type, чтоб подставить туда TempVar
        -- В идеале сделать список всех подстановок, но я пока не придумал как это сделать
        debugPrintT $ "Return type: " <> showT returnType
        debugPrintT $ "Result type: " <> showT resultType
        let
          finalTypeVarsMapping = typeVarsMapping1 ++ typeVarsMapping2 ++ typeVarsMapping3
          funcTerm'' = applyTypeVarsMapping scope funcTerm' finalTypeVarsMapping
          resultType' = applyTypeVarsMapping scope resultType finalTypeVarsMapping

        pure (App funcTerm'' argTerm', resultType', finalTypeVarsMapping)
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
    (lTerm', typeVarsMapping1) <- check scope env lTerm argTyp
    (rTerm', typeVarsMapping2) <- check scope env rTerm argTyp
    let finalTypeVarsMapping = typeVarsMapping1 ++ typeVarsMapping2
    pure (OpExpr lTerm' op rTerm', retTyp, finalTypeVarsMapping)

  Boolean b -> do
    typ <- extendTypeToCurrentScope scope $ F.sink $ boolWithT b
    pure (currentTerm, typ, [])

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
    (termWIthTLam, typeVarsMapping) <- mkTLamIfNecessary scope env term extAnnType
    pure (Ann annType termWIthTLam, extAnnType, typeVarsMapping)

  _ -> throwError $ "unimplemented case: " <> showT currentTerm

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
        newTempTypVar <- mkFreshTempTypVar >>= extendTypeToCurrentScope scope . F.sink
        let
          resultTypeSubst = F.addSubst F.identitySubst typVarBinder newTempTypVar
          F.UnsafeName rawTypVarBinder = F.nameOf typVarBinder
        debugPrintT $ "Elab substTypeVar: " <> showT typVarBinder <> " -> " <> showT newTempTypVar
        resultType <- extendTypeToCurrentScope scope $
            substTypeVar scope resultTypeSubst rawTypVarBinder typUnderForall
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
  CheckerM (Term i {- new term with TApp -}, TempTypeVarUnification i)
mkTLamIfNecessary scope env term typ = case typ of
  TypeForall typVar typUnderForall ->
    case (F.assertDistinct typVar, F.assertExt typVar) of
      (F.Distinct, F.Ext) -> do
        let
          scope' = F.extendScopePattern typVar scope
        -- на самом деле нам тут env расширять не нужно, но скоупы есть скоупы
        (term', typeVarsMapping)  <- withExtendedEnv env (getNameBinderFromPattern typVar) (F.sink anyIntT) $ \env' ->
          mkTLamIfNecessary scope' env' (F.sink term) typUnderForall
        let term'' = applyTypeVarsMapping scope' term' typeVarsMapping
        pure (TLam typVar term'', unsinkTypeVarsMapping (getNameBinderFromPattern typVar) typeVarsMapping)
  {-
  Есть некоторый нюанс в том что если мы спускаем term i вниз через sink i1 -> i2 -> i3.
  И в этих скоупах добавляются новые биндеры, то у изначально терма i со своими биндерами i1 -> i2 -> i3,
  может быть конфликт имен. Поэтому надо рефрешнуть все биндеру снизу у изначального терма

  Проблема что check вызывается здесь а не в Syn-Ann
  Forall накидывает скоупы, из-за этого мы не можем тип под forall поднять на верх
  решение - вызвать check term annType внутри mkTLamIfNecessary
  -}
  _ -> check scope env term typ
