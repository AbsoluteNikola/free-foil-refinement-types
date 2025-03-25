module Language.Sprite.Syntax.Convert.FrontToInner where

import Language.Sprite.Syntax.Front.Abs qualified as F
import Language.Sprite.Syntax.Inner.Abs qualified as I
import Data.List (nub)
import Data.Functor ((<&>))

data ConvertError
  = DifferentNameForBindingAndAnnotationError
      F.VarIdent {- Annotation identifier -}
      F.VarIdent {- Binding identifier -}
      F.Decl {- Part of code -}
  deriving (Show) -- TODO: add pretty printer

convert :: F.Term -> Either ConvertError I.Term
convert ft = case ft of
  F.ConstInt i ->
    pure $ I.ConstInt i
  F.Bool b -> pure $ I.Boolean (convertConstBool b)
  F.If cond ifB elseB -> do
    ifB' <- convert ifB
    elseB' <- convert elseB
    pure $ I.If (convertFuncAppArg cond) ifB' elseB'
  F.Var varId -> pure $ I.Var (convertVarId varId)
  F.Let decl body -> do
    (I.ScopedTerm -> convertedBody) <- convert body
    let_ <- convertDecl decl
    pure $ let_ convertedBody
  F.Fun args body -> do
    convertedBody <- convert body
    let
      funs = foldr
        (\(F.FunArgName argId) t -> I.Fun (convertVarIdToPattern argId) (I.ScopedTerm t))
        convertedBody
        args
    pure funs
  F.ConApp conName args -> do
    let
      convertedConName = I.Constructor $ convertConId conName
      args' = case args of
        F.EmptyConAppArgs -> []
        F.NonEmptyConAppArgs some -> some
      apps = foldl
        (\t arg -> I.App t (convertFuncAppArg arg))
        convertedConName
        args'
    pure apps
  F.FunApp funName args -> do
    let
      convertedFuncTerm = I.Var $ convertVarId funName
      apps = foldl
        (\t arg -> I.App t (convertFuncAppArg arg))
        convertedFuncTerm
        args
    pure apps
  F.Op lArg op rArg -> pure $
    op_ (convertFuncAppArg lArg) (convertFuncAppArg rArg)
    where
      op_ l r = case op of
        F.IntPlus -> I.OpExpr l I.PlusOp r
        F.IntMinus -> I.OpExpr l I.MinusOp r
        F.IntMultiply -> I.OpExpr l I.MultiplyOp r
        F.IntEq -> I.OpExpr l I.EqOp r
        F.IntLessThan -> I.OpExpr l I.LessOp r
        F.IntLessOrEqThan -> I.OpExpr l I.LessOrEqOp r
        F.IntGreaterThan -> I.OpExpr l I.GreaterOp r
        F.IntGreaterOrEqThan -> I.OpExpr l I.GreaterOrEqOp r
  F.Switch arg cases -> do
    convertedCases <- traverse convertSwitchCase cases
    pure $
      I.Switch (I.Var $ convertVarId arg) convertedCases

convertSwitchCase :: F.SwitchCase -> Either ConvertError I.Term
convertSwitchCase (F.SwitchCase conName frontConArgs term) = do
  convertedTerm <- convert term
  let
    innerConArgs = case frontConArgs of
      F.SwitchCaseEmptyDataConArgs -> I.PatternNoBinders
      F.SwitchCaseNonEmptyDataConArgs args -> foldr
        (\(F.FunArgName argId) b -> I.PatternSomeBinders (convertVarId argId) b)
        I.PatternNoBinders
        args
  pure $ I.CaseAlt (convertVarId conName) innerConArgs (I.ScopedTerm convertedTerm)

convertVarId :: F.VarIdent -> I.VarIdent
convertVarId (F.VarIdent varId) = I.VarIdent varId

convertConId :: F.ConIdent -> I.ConIdent
convertConId (F.ConIdent varId) = I.ConIdent varId

convertVarIdToPattern :: F.VarIdent -> I.Pattern
convertVarIdToPattern (F.VarIdent varId) = I.PatternVar $ I.VarIdent varId

convertFuncAppArg :: F.FuncAppArg -> I.Term
convertFuncAppArg = \case
  F.FuncAppArgInt n -> I.ConstInt n
  F.FuncAppArgVar varId -> I.Var (convertVarId varId)
  F.FuncAppArgBool b -> I.Boolean (convertConstBool b)

convertDecl :: F.Decl -> Either ConvertError (I.ScopedTerm -> I.Term)
convertDecl decl = case decl of
  F.AnnotatedDecl (F.Annotation annId typ) varId varValue
    | annId /= varId -> Left $
      DifferentNameForBindingAndAnnotationError annId varId decl
    | otherwise -> do
      convertedVarValue <- convert varValue
      let annTyp = mkForAll typ $ convertRType typ
      pure $ \letBody ->
        I.Let (convertVarIdToPattern varId) (I.Ann annTyp convertedVarValue) letBody
  F.UnAnnotatedDecl varId varValue ->  do
      convertedVarValue <- convert varValue
      pure $ \letBody ->
        I.Let (convertVarIdToPattern varId) convertedVarValue  letBody
  F.RecDecl (F.Annotation annId typ) varId varValue
    | annId /= varId -> Left $
        DifferentNameForBindingAndAnnotationError annId varId decl
    | otherwise -> do
      convertedVarValue <- convert varValue
      let annTyp = mkForAll typ $ convertRType typ
      pure $ \letBody ->
        I.LetRec annTyp (convertVarIdToPattern varId) (I.ScopedTerm convertedVarValue) letBody

convertRType :: F.RType -> I.Term
convertRType rType = case rType of
  F.TypeRefined base ref ->
    let
      (v, predicate) = convertRefinement ref
    in I.TypeRefined (convertBaseType base) v predicate

  F.TypeFun (F.NamedFuncArg argId argType) retType ->
    I.TypeFun
      (convertVarIdToPattern argId)
      (convertRType argType)
      (I.ScopedTerm $ convertRType retType)
  F.TypeFun (F.UnNamedFuncArg argType) retType ->
    I.TypeFun
      (I.PatternVar $ I.VarIdent "_arg")
      (convertRType argType)
      (I.ScopedTerm $ convertRType retType)
  F.TypeData typId typArgs ref ->
    let
      (refVar, refPred) = convertRefinement ref
      convertedTypArgs = case typArgs of
        F.NonEmptyTypeDataArgs args -> I.NonEmptyTypeDataArgs $
          args <&> \(F.TypeDataArg argTyp) -> I.TypeDataArg (convertRType argTyp)
        F.EmptyTypeDataArgs -> I.EmptyTypeDataArgs
    in I.TypeData (convertVarId typId) convertedTypArgs refVar refPred

convertRefinement :: F.Refinement -> (I.Pattern, I.ScopedTerm)
convertRefinement = \case
  F.KnownRefinement var p -> (convertVarIdToPattern var, I.ScopedTerm $ convertPredicate p)
  F.UnknownRefinement -> (I.PatternVar $ I.VarIdent "v", I.ScopedTerm I.Unknown)
  F.SimpleRefinement -> (I.PatternVar $ I.VarIdent "v", I.ScopedTerm $ I.Boolean I.ConstTrue)

convertBaseType :: F.BaseType -> I.Term
convertBaseType = \case
  F.BaseTypeInt -> I.BaseTypeInt
  F.BaseTypeBool -> I.BaseTypeBool
  F.BaseTypeVar (F.TypeVarId v) -> (I.BaseTypeVar $ I.Var (convertVarId v))


convertConstBool :: F.ConstBool -> I.ConstBool
convertConstBool = \case
  F.ConstTrue -> I.ConstTrue
  F.ConstFalse -> I.ConstFalse

convertPredicate :: F.Pred -> I.Term
convertPredicate predicate = case predicate of
  F.PVar varId -> I.Var (convertVarId varId)
  F.PBool b -> I.Boolean $ convertConstBool b
  F.PInt n -> I.ConstInt n
  F.PEq l r -> I.OpExpr (convertPredicate l) I.EqOp (convertPredicate r)
  F.PLessThan l r ->  I.OpExpr (convertPredicate l) I.LessOp (convertPredicate r)
  F.PLessOrEqThan l r ->  I.OpExpr (convertPredicate l) I.LessOrEqOp (convertPredicate r)
  F.PGreaterThan l r ->  I.OpExpr (convertPredicate l) I.GreaterOp (convertPredicate r)
  F.PGreaterOrEqThan l r ->  I.OpExpr (convertPredicate l) I.GreaterOrEqOp (convertPredicate r)
  F.PPlus l r ->  I.OpExpr (convertPredicate l) I.PlusOp (convertPredicate r)
  F.PMinus l r ->  I.OpExpr (convertPredicate l) I.MinusOp (convertPredicate r)
  F.PMultiply l r ->  I.OpExpr (convertPredicate l) I.MultiplyOp (convertPredicate r)
  F.POr l r ->  I.OpExpr (convertPredicate l) I.OrOp (convertPredicate r)
  F.PAnd l r ->  I.OpExpr (convertPredicate l) I.AndOp (convertPredicate r)
  F.PMeasure measureId args -> I.Measure (convertVarId measureId) $
    args <&> \(F.FunArgName arg) -> I.Var (convertVarId arg)

mkSimpleType :: I.Term -> I.Term
mkSimpleType base = I.TypeRefined base (I.PatternVar "v") (I.ScopedTerm $ I.Boolean I.ConstTrue)

collectFreeVars :: F.RType -> [I.VarIdent]
collectFreeVars = nub . map convertVarId . go
  where
    go = \case
      F.TypeFun (F.NamedFuncArg _ argType) retType -> go argType ++ go retType
      F.TypeFun (F.UnNamedFuncArg argType) retType -> go argType ++ go retType
      F.TypeRefined b _ -> fromBase b
      F.TypeData _ typArgs _ -> fromTypeDataArgs typArgs

    fromTypeDataArgs = \case
      F.EmptyTypeDataArgs -> []
      F.NonEmptyTypeDataArgs args -> flip concatMap args $
        \(F.TypeDataArg arg) -> go arg

    fromBase = \case
      F.BaseTypeVar (F.TypeVarId v) -> [v]
      _ -> []

mkForAll :: F.RType -> I.Term -> I.Term
mkForAll typ term = foldr f term freeVars
  where
    freeVars = collectFreeVars typ
    f typeVar curTerm = I.TypeForall (I.PatternVar typeVar) (I.ScopedTerm curTerm)
