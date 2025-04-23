{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}

module Language.Sprite.TypeCheck.Check where
import Control.Monad.Foil qualified as F
import Control.Monad.Foil.Internal qualified as F
import Control.Monad.Free.Foil qualified as F
import Language.Sprite.Syntax
import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Language.Sprite.TypeCheck.Constraints
import qualified Language.Sprite.Syntax.Inner.Abs as Inner
import Language.Sprite.TypeCheck.Predicates
import Language.Sprite.TypeCheck.Types
import qualified Data.Unique as Unique
import Language.Sprite.TypeCheck.Monad
import Data.Traversable (for)
import Data.Bifunctor (bimap)
import qualified Data.List as List
import Control.Monad (foldM)
import Language.Refinements.Constraint (withExtendedEnv, envToList, changeVarTypeInEnv)
import Language.Refinements.Constraint (lookupEnv)
import qualified Language.Refinements.Constraint as LR

mkSolverErrorMessage :: Text -> CheckerM Text
mkSolverErrorMessage baseMsg = do
  (Unique.hashUnique -> errId) <- liftIO Unique.newUnique
  debugPrintT $ "Set error with id " <> showT errId
  pure $ baseMsg <> "\nError id: " <> showT errId

subtype :: F.Distinct i => F.Scope i -> Term  i -> Term i -> CheckerM LR.Constraint

{- | [Sub-Base]
  Γ ⊢ ∀ (v : t). p => q[w := v]
  ————————————————————————————— Sub-Base
  Γ ⊢ b{v:p} <: b{w:q}
 -}
subtype scope lt@(TypeRefined lb leftVarPat@(PatternVar v) _) (TypeRefined rb rightVarPat@(PatternVar w) rightPredicate)
  | not (baseTypeEq lb rb) = throwError
    $ "Invalid subtyping. Different refinement base: " <> pShowT lb <> " and " <> pShowT rb
  | otherwise = withRule "[Sub-Base]" $ do
      case (F.assertDistinct leftVarPat, F.assertExt leftVarPat) of
        (F.Distinct, F.Ext) -> do
          implMsg <- mkSolverErrorMessage $ "Refinement subtype error: (v::t) => q[w := v]. Where v="
            <> showT leftVarPat
            <> ", w=" <> showT rightVarPat
          predMsg <- mkSolverErrorMessage $ "Refinement subtype error: q[w := v]. Where w="
            <> showT rightVarPat <> " and v=" <> showT leftVarPat
          let
            scope' = F.extendScopePattern leftVarPat scope
            subst = F.addRename (F.sink F.identitySubst) w (F.nameOf v)
            rightPredicate' = F.substitute scope' subst rightPredicate
            conclusionPred = LR.cPred rightPredicate' predMsg
          pure $ LR.сImplicationFromType  scope (getNameBinderFromPattern leftVarPat) lt conclusionPred implMsg

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
      implMsg <- mkSolverErrorMessage $ "Function subtype error: x2:s2 |- t1[x1:=x2] <: t2. Where x2="  <> showT rightFunArgPat
      debugPrintT $ "rightArgPat = " <> showT rightFunArgPat <> ", leftFunRetT' = " <> showT leftFunRetT
      let
        returnTypesConstraints = LR.сImplicationFromType
          scope
          (getNameBinderFromPattern rightFunArgPat)
          rightFunArgT
          returnTypesSubtypingConstraints implMsg
      pure $ LR.cAnd [argSubtypingConstrains, returnTypesConstraints]

{-
G,v:p |- q[w:=v]     G |- si <: ti
-----------------------------------------
G |- (C s1...)[v|p] <: (C t1...)[w|q]
-}
subtype scope
  lt@(TypeData typeNameL typeArgsL typVarL _typPredL)
  (TypeData typeNameR typeArgsR typVarR typPredR)
  | typeNameL == typeNameR
  , length typeArgsL == length typeArgsR = do
    (LR.cAnd -> argsConstraints) <- traverse (\(s1, s2) -> subtype scope s1 s2) (zip typeArgsL typeArgsR)
    case (F.assertDistinct typVarL, F.assertExt typVarL) of
      (F.Distinct, F.Ext) -> do
        implMsg <- mkSolverErrorMessage $ "type data subtype error: (v::t) => q[w := v]. Where v="
          <> showT typVarL
          <> ", w=" <> showT typVarL
        predMsg <- mkSolverErrorMessage $ "type data subtype error: q[w := v]. Where w="
          <> showT typVarR <> " and v=" <> showT typVarL
        let
          scope' = F.extendScopePattern typVarL scope
          subst = F.addRename
            (F.sink F.identitySubst)
            (getNameBinderFromPattern typVarR)
            (F.nameOf $ getNameBinderFromPattern typVarL)
          rightPredicate' = F.substitute scope' subst typPredR
          conclusionPred = LR.cPred rightPredicate' predMsg
        let implPred = LR.сImplicationFromType scope (getNameBinderFromPattern typVarL) lt conclusionPred  implMsg
        pure $ LR.cAnd [argsConstraints, implPred]

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
  CheckerM LR.Constraint
check scope env currentTerm currentType = case currentTerm of
  {- [Chk-Lam]
  G, x:s |- e[y := x] <== t
  --------------------------
  G |- \y.e <== x:s -> t
  -}
  Fun varPattern body -> withRule "[Chk-Lam]" $ do
    extendedCurrentType <- extendTypeToCurrentScope scope currentType
    case extendedCurrentType of
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

            bodyCheckConstraints <- withExtendedEnv env typeFunArgIdBinder argType $ \env' ->
              check scope' env' bodySubstituted returnType
            debugPrintT $ "Arg type: " <> showT argType
            implMsg <- mkSolverErrorMessage "Checking func error"
            pure $ LR.сImplicationFromType
              scope
              (getNameBinderFromPattern typeFunArgIdPat)
              argType
              bodyCheckConstraints
              implMsg
      _ -> throwError $ "Function type should be Function, not: " <> pShowT extendedCurrentType

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
          extendedCurrentType <- extendTypeToCurrentScope scope' (F.sink currentType)
          bodyConstraints <- check scope' env' body extendedCurrentType
          implMsg <- mkSolverErrorMessage "Checking let error"
          let
            implLetBodyConstraint = LR.сImplicationFromType
              scope
              (getNameBinderFromPattern newVarPat)
              newVarType
              bodyConstraints
              implMsg
          pure $ LR.cAnd [newVarConstraints, implLetBodyConstraint]

  {- [Chk-Rec]
    s1 |> t1
    G |- t1 : k
    G, x:t1 |- e1 <== t1
    G, x:t1 |- e2 <== t2
    ----------------------------------------------
        G |- let rec x = e1 : s1 in e2 <== t2
  -}
  LetRec newVarType newVarPat1 newVarPat2 newVarValue body -> withRule "[Chk-Rec]" $ do
    debugPrintT $  "new var type: " <> showT newVarType
    freshedNewVarType <- fresh scope env newVarType
    debugPrintT $  "freshed new var type: " <> showT freshedNewVarType
    -- G |- t1 : k, t1 - newVarType
    case (F.assertDistinct newVarPat1, F.assertDistinct newVarPat2, F.assertExt newVarPat1,  F.assertExt newVarPat2) of
      (F.Distinct, F.Distinct, F.Ext, F.Ext) ->  do
        --  G, x:t1 |- e1 <== t1
        debugPrintT $  "check new var: " <> showT newVarPat1
        newVarC <- withExtendedEnv env (getNameBinderFromPattern newVarPat1) freshedNewVarType $ \env' -> do
          let
            scope' = F.extendScopePattern newVarPat1 scope
          newVarConstraints <- check scope' env' newVarValue =<< extendTypeToCurrentScope scope' (F.sink freshedNewVarType)
          implBodyMsg <- mkSolverErrorMessage "Chk-Rec: new var"
          pure $ LR.сImplicationFromType
            scope
            (getNameBinderFromPattern newVarPat1)
            freshedNewVarType
            newVarConstraints
            implBodyMsg
        -- G, x:t1 |- e2 <== t2
        bodyC <- withExtendedEnv env (getNameBinderFromPattern newVarPat2) freshedNewVarType $ \env' -> do
          let scope' = F.extendScopePattern newVarPat2 scope
          extendedCurrentType <- extendTypeToCurrentScope scope' (F.sink currentType)
          bodyConstraints <- check scope' env' body extendedCurrentType
          implBodyMsg <- mkSolverErrorMessage "Chk-Rec: body"
          pure $ LR.сImplicationFromType
            scope
            (getNameBinderFromPattern newVarPat2)
            freshedNewVarType
            bodyConstraints
            implBodyMsg
        pure $ LR.cAnd [newVarC, bodyC]

  {- [Chk-If]
  y is fresh, need to pass condition value to constraints,

  Γ ⊢  <== bool    G, y:int[y|x] |- e1 <== t     G, y:int[y|not x]  |- e2 <== t
  -----------------------------------------------------------------------------
      G |- if x then e1 else e2 <== t
  -}
  If cond thenTerm elseTerm -> withRule "[Chk-If]" $ do
    condCheckConstraints <- check scope env cond (F.sink anyBoolT)
    debugPrintT "Left branch"
    thenConstraints <- mkIfBranchCheck Inner.ConstTrue thenTerm
      "type check If error in then branch"
    debugPrintT "Right branch"
    elseConstraints <- mkIfBranchCheck Inner.ConstFalse elseTerm
      "type check If error in else branch"

    pure $ LR.cAnd [condCheckConstraints, thenConstraints, elseConstraints]
    where
      mkIfBranchCheck condShouldBe brachTerm message = do
        F.withFreshBinder scope $ \freshY ->
          case (F.assertDistinct freshY, F.assertExt freshY) of
            (F.Distinct, F.Ext) ->
              withExtendedEnv env freshY (F.sink anyBoolT) $ \env' -> do
                let
                  scope' = F.extendScope freshY scope
                  t = TypeRefined
                    BaseTypeBool
                    (PatternVar freshY)
                    (OpExpr (F.sink cond)
                      Inner.EqOp
                      (Boolean condShouldBe))
                extendedCurrentType <- extendTypeToCurrentScope scope' (F.sink currentType)
                branchCheckConstraints <- check scope' env' (F.refreshAST scope' $ F.sink brachTerm) extendedCurrentType
                pure $ LR.сImplicationFromType scope freshY (F.sink t) branchCheckConstraints message
  {- [Chk-TLam]

  G, a |- e <== t
  ------------------------ [Chk-TLam]
  G |- Λ a. e <== all a. t
  -}
  TLam typAppVar body -> withRule "[Chk-TLam]" $ do
    case currentType of
      TypeForall forAllVar typUnderForall -> do
        case (F.assertDistinct forAllVar, F.assertExt forAllVar) of
          (F.Distinct, F.Ext) -> do
            let
                scope' = F.extendScopePattern forAllVar scope
                subst = F.addRename
                  (F.sink F.identitySubst)
                  (getNameBinderFromPattern typAppVar)
                  (F.nameOf $ getNameBinderFromPattern forAllVar)
                body' = F.substitute scope' subst body
            -- Нужно чем-то расширить env до нужного скоупа, поэтому просто записываем туда Unknown
            -- Unknown для этого не предназначен, но выстрелит если это расширение env будет где-то использоваться
            withExtendedEnv env (getNameBinderFromPattern forAllVar) Unknown $ \env' ->
              check scope' env' body' typUnderForall
      _ -> throwError $ "TLam with type without forall: " <> showT currentType

  Switch (F.Var varName) cases -> do
    varTyp <- extendTypeToCurrentScope scope $ lookupEnv env varName

    debugPrintT $ "switch var type" <> showT varTyp
    caseConstraints <- for cases $ \case
      CaseAlt conName@(Inner.ConIdent conNameRaw) newVarsPat caseTerm ->
        case (F.assertDistinct newVarsPat, F.assertExt newVarsPat) of
          (F.Distinct, F.Ext) -> do
            conType <- lookupConstructor conName >>= extendTypeToCurrentScope scope . F.sink
            constructorFunction <- ctor scope varTyp conType
            (newEnv, ctorType) <- unapply scope env newVarsPat constructorFunction
            let
              scope' = F.extendScopePattern newVarsPat scope
              varsAddedInCasePatternMatch = List.deleteFirstsBy
                (\(n1, _) (n2, _) -> n1 == n2)
                (envToList newEnv)
                (bimap F.sink F.sink <$> envToList env)
              msg = "Error in switch case in branch with "  <> showT conNameRaw

            -- В теории можно было бы расширить scope еще одним биндером, переименовать
            -- все вхождения varName на новый биндер и новый биндер добавить в env c усиленным типом,
            -- но я пошел простым путем. Просто перезатер varName в env. В smt это будет shadowing varName
            -- Решение принято осознанное
            strengthenVarType <- meet scope' (F.sink varTyp) ctorType
            let envWithStrengthenVar = changeVarTypeInEnv newEnv (F.sink varName) strengthenVarType
            debugPrintT $ "strengthenVarType: " <> showT varName <> showT strengthenVarType
            caseConstraint <- check
              (F.extendScopePattern newVarsPat scope)
              envWithStrengthenVar
              caseTerm
              (F.sink currentType)
            implConstraints <- foldM
              (\c (addedVarName, addedVarType) ->
                buildImplicationFromType' msg scope' addedVarName addedVarType c
                -- pure $ LR.сImplicationFromType scope
              )
              caseConstraint
              ((F.sink varName, strengthenVarType) : varsAddedInCasePatternMatch)
            pure implConstraints
      otherTerm -> throwError $ "switch case not CaseAlt: " <> showT otherTerm
    pure $ LR.cAnd caseConstraints


  {- [Chk-Syn]
  G |- e ==> s        G |- s <: t
  ----------------------------------[Chk-Syn]
            G |- e <== t
  -}
  term -> withRule "[Chk-Syn]" $ do
    (synthsConstraints, termType) <- synths scope env term
    extendedCurrentType <- extendTypeToCurrentScope scope currentType
    debugPrintT "on term" >> debugPrint term
    debugPrintT "calling subtype"
    debugPrintT "left type: " >> debugPrint termType
    debugPrintT "right type: " >> debugPrint extendedCurrentType
    subtypingConstraints <- subtype scope termType extendedCurrentType
    pure $ LR.cAnd [synthsConstraints, subtypingConstraints]

synths ::
  (F.DExt F.VoidS i) =>
  F.Scope i ->
  Env i ->
  Term i ->
  CheckerM (LR.Constraint, Term i)
synths scope env currentTerm = case currentTerm of
  {- [Syn-Var]
    G(x) = t
   ----------------------
    G |- x ==> self(x, t)
  -}
  F.Var varId -> withRule "[Syn-Var]" $ do
    let
      typ = LR.lookupEnvWithStrengthening scope env varId
    debugPrintT $ "lookup: " <> showT currentTerm
    debugPrintT $ "typ: " <> showT typ
    pure (LR.cTrue, typ)

  Constructor conName -> withRule "[Syn-Constructor]" $ do
    typ <- lookupConstructor conName
    typExtended <- extendTypeToCurrentScope scope (F.sink typ)
    pure (LR.cTrue, typExtended)

  {- [Syn-Con]
   -----------------
    G |- x ==> G(x)
  -}
  ConstInt x -> withRule "[Syn-Con]" $ do
    debugPrintT "type: " >> debugPrint (constIntT x)
    extendedConstInt <- extendTypeToCurrentScope scope (F.sink (constIntT x))
    pure (LR.cTrue, extendedConstInt)

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
        debugPrintT $ "fun type: " <> showT funcType
        debugPrintT "argument term: " >> debugPrint argTerm
        debugPrintT "argument type: " >> debugPrint varType
        argConstraints <- check scope env argTerm varType
        let
          resultType =
            F.substitutePattern scope F.identitySubst varPattern [argTerm] returnType
        debugPrintT "resultType: " >> debugPrint resultType
        pure (LR.cAnd [funcConstraints, argConstraints], resultType)
      _ -> throwError "Application to non function"

  {- [Syn-TApp]
  G |- e ==> forall a. t1 |> t2
  ---------------------------
  G |- e[t1] ==> s [ a := t2]
  -}
  TApp funTerm typ -> withRule "[Syn-TApp]" $ do
    (cSyn, funTyp) <- synths scope env funTerm
    freshedTyp <- fresh scope env typ
    case funTyp of
      TypeForall typVar typUnderForall -> do
        let
          subst = F.addSubst F.identitySubst (getNameBinderFromPattern typVar) freshedTyp
          F.UnsafeName rawTypVarName = F.nameOf $ getNameBinderFromPattern typVar
        let typUnderForall' = substTypeVar scope subst rawTypVarName typUnderForall
        debugPrintT $ "Check substTypeVar: " <> showT typVar <> " -> " <> showT freshedTyp
        debugPrintT $ "Was: " <> showT funTyp
        debugPrintT $ "Change: " <> showT typVar <> " on " <> showT freshedTyp
        debugPrintT $ "Became: " <> showT typUnderForall'
        pure (cSyn, typUnderForall')
      _ -> throwError $ "type application to non forall: " <> showT funTyp

  {- [Syn-Ann]
   G |- e <== t
   -----------------
   G |- e:t => t
  -}
  Ann annType term -> withRule "[Syn-Ann]" $ do
    debugPrintT $ "Freshing: " <> showT annType
    freshedAnnType <- fresh scope env annType
    debugPrintT $ "Freshed: " <> showT annType
    extendedAnnType <- extendTypeToCurrentScope scope freshedAnnType
    checkConstraints <- check scope env term extendedAnnType
    pure (checkConstraints, extendedAnnType)

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
      pure (LR.cAnd [leftTermConstraints, rightTermConstraints], substitutedReturnType)
  Boolean b -> do
    typ <- extendTypeToCurrentScope scope $ F.sink $ boolWithT b
    pure (LR.cTrue, typ)

  _ -> throwError $ "unimplemented case:\n" <> showT currentTerm
