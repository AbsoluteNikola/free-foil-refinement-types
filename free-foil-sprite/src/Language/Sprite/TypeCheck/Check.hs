{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Sprite.TypeCheck.Check where
import Control.Monad.Foil qualified as F
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

mkSolverErrorMessage :: Text -> CheckerM Text
mkSolverErrorMessage baseMsg = do
  (Unique.hashUnique -> errId) <- liftIO Unique.newUnique
  debugPrintT $ "Set error with id " <> showT errId
  pure $ baseMsg <> "\nError id: " <> showT errId

subtype :: F.Distinct i => F.Scope i -> Term  i -> Term i -> CheckerM Constraint

{- | [Sub-Base]
  Γ ⊢ ∀ (v : t). p => q[w := v]
  ————————————————————————————— Sub-Base
  Γ ⊢ b{v:p} <: b{w:q}
 -}
subtype scope lt@(TypeRefined lb leftVarPat@(PatternVar v) _) (TypeRefined rb rightVarPat@(PatternVar w) rightPredicate)
  | lb /= rb = throwError
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
      implMsg <- mkSolverErrorMessage $ "Function subtype error: x2:s2 |- t1[x1:=x2] <: t2. Where x2="  <> showT rightFunArgPat
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

            bodyCheckConstraints :: Constraint <- withExtendedEnv env typeFunArgIdBinder argType $ \env' ->
              check scope' env' bodySubstituted returnType
            debugPrintT $ "Arg type: " <> showT argType
            implMsg <- mkSolverErrorMessage "Checking func error"
            buildImplicationFromType implMsg scope' typeFunArgIdPat (F.sink argType) bodyCheckConstraints
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
          implLetBodyConstraint <-
            buildImplicationFromType implMsg scope' newVarPat (F.sink newVarType) bodyConstraints

          pure $ CAnd [newVarConstraints, implLetBodyConstraint]

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
          newVarConstraints <- check scope' env' newVarValue (F.sink freshedNewVarType)
          implBodyMsg <- mkSolverErrorMessage "Chk-Rec: new var"
          buildImplicationFromType implBodyMsg scope' newVarPat1 (F.sink freshedNewVarType) newVarConstraints
        -- G, x:t1 |- e2 <== t2
        bodyC <- withExtendedEnv env (getNameBinderFromPattern newVarPat2) freshedNewVarType $ \env' -> do
          let scope' = F.extendScopePattern newVarPat2 scope
          extendedCurrentType <- extendTypeToCurrentScope scope' (F.sink currentType)
          bodyConstraints <- check scope' env' body extendedCurrentType
          implBodyMsg <- mkSolverErrorMessage "Chk-Rec: body"
          buildImplicationFromType implBodyMsg scope' newVarPat2 (F.sink freshedNewVarType) bodyConstraints
        pure $ CAnd [newVarC, bodyC]

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

    pure $ CAnd [condCheckConstraints, thenConstraints, elseConstraints]
    where
      mkIfBranchCheck condShouldBe brachTerm message = do
        F.withFreshBinder scope $ \freshY ->
          case (F.assertDistinct freshY, F.assertExt freshY) of
            (F.Distinct, F.Ext) ->
              withExtendedEnv env freshY (F.sink anyBoolT) $ \env' -> do
                let
                  scope' = F.extendScope freshY scope
                  t = TypeRefined
                    Inner.BaseTypeBool
                    (PatternVar freshY)
                    (OpExpr (F.sink cond)
                      Inner.EqOp
                      (Boolean condShouldBe))
                extendedCurrentType <- extendTypeToCurrentScope scope' (F.sink currentType)
                branchCheckConstraints <- check scope' env' (F.sink brachTerm) extendedCurrentType
                buildImplicationFromType message scope' (PatternVar freshY) (F.sink t) branchCheckConstraints

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
    G(x) = t
   ----------------------
    G |- x ==> self(x, t)
  -}
  F.Var varId -> withRule "[Syn-Var]" $ do
    let
      typ = lookupEnv env varId
    typExtended <- extendTypeToCurrentScope scope typ
    let
      typWithSelf = singletonT varId typExtended
    debugPrintT $ "lookup: " <> showT currentTerm
    debugPrintT $ "typ: " <> showT typWithSelf
    pure (cTrue, typWithSelf)

  {- [Syn-Con]
   -----------------
    G |- x ==> G(x)
  -}
  ConstInt x -> withRule "[Syn-Con]" $ do
    debugPrintT "type: " >> debugPrint (constIntT x)
    extendedConstInt <- extendTypeToCurrentScope scope (F.sink (constIntT x))
    pure (cTrue, extendedConstInt)

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
    freshedAnnType <- fresh scope env annType
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
      pure (CAnd [leftTermConstraints, rightTermConstraints], substitutedReturnType)
  _ -> error "unimplemented case"

getNameBinderFromPattern :: Pattern i o -> F.NameBinder i o
getNameBinderFromPattern (PatternVar binder) = binder

getRawVarIdFromPattern :: Pattern i o -> Inner.VarIdent
getRawVarIdFromPattern varPat = case fromPattern varPat of
  Inner.PatternVar v -> v

extendTypeToCurrentScope :: F.Distinct i => F.Scope i -> Term i -> CheckerM (Term i)
extendTypeToCurrentScope scope typ = case typ of
  TypeRefined b oldVar p -> F.withFreshBinder scope $ \newBinder ->
    case (F.assertDistinct newBinder, F.assertExt newBinder) of
      (F.Distinct, F.Ext) -> do
        let
          scope' = F.extendScope newBinder scope
          newPred = F.substitutePattern scope' (F.sink F.identitySubst) oldVar [F.Var (F.nameOf newBinder)] p
        pure $ TypeRefined b (PatternVar newBinder) newPred
  TypeFun argName argTyp retTyp ->  F.withFreshBinder scope $ \newBinder ->
    case (F.assertDistinct newBinder, F.assertExt newBinder) of
      (F.Distinct, F.Ext) -> do
        argTypExtended <- extendTypeToCurrentScope scope argTyp
        let
          scope' = F.extendScope newBinder scope
          newRetType = F.substitutePattern scope' (F.sink F.identitySubst) argName [F.Var (F.nameOf newBinder)] retTyp
        newRetTypeExtended <- extendTypeToCurrentScope scope' newRetType
        pure $ TypeFun (PatternVar newBinder) argTypExtended newRetTypeExtended
  _ -> throwError $
    "extendTypeToCurrentScope should be called only on type, not term\n"
    <> showT typ
