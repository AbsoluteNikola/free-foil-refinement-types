module Language.Sprite.Syntax.Convert.FrontToInner where

import Language.Sprite.Syntax.Front.Abs qualified as F
import Language.Sprite.Syntax.Inner.Abs qualified as I
import Data.List (nub)
import Data.Foldable (foldl')

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
  F.Fun argId body -> do
    (I.ScopedTerm -> convertedBody) <- convert body
    pure $ I.Fun (convertVarIdToPattern argId) convertedBody
  F.App funcName argTerm -> do
    let convertedFuncTerm = I.Var $ convertVarId funcName
    let convertedArgTerm = convertFuncAppArg argTerm
    pure $ I.App convertedFuncTerm convertedArgTerm
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

convertVarId :: F.VarIdent -> I.VarIdent
convertVarId (F.VarIdent varId) = I.VarIdent varId

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
  F.TypeRefined base varId predicate ->
    I.TypeRefined
      (convertBaseType base)
      (convertVarIdToPattern varId)
      (I.ScopedTerm $ convertPredicate predicate)

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
  F.TypeVar varId -> mkSimpleType (I.BaseTypeVar $ I.Var (convertVarId varId))
  F.TypeRefinedUnknown base -> I.TypeRefinedUnknown (convertBaseType base)
  F.TypeRefinedSimple base -> mkSimpleType (convertBaseType base)

convertBaseType :: F.BaseType -> I.Term
convertBaseType = \case
  F.BaseTypeInt -> I.BaseTypeInt
  F.BaseTypeBool -> I.BaseTypeBool

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

mkSimpleType :: I.Term -> I.Term
mkSimpleType base = I.TypeRefined base (I.PatternVar "v") (I.ScopedTerm $ I.Boolean I.ConstTrue)

collectFreeVars :: F.RType -> [I.VarIdent]
collectFreeVars = nub . map convertVarId . go
  where
    go = \case
      F.TypeRefined{} -> []
      F.TypeFun (F.NamedFuncArg _ argType) retType -> go argType ++ go retType
      F.TypeFun (F.UnNamedFuncArg argType) retType -> go argType ++ go retType
      F.TypeVar varId -> [varId]
      F.TypeRefinedUnknown _ -> []
      F.TypeRefinedSimple _ -> []

mkForAll :: F.RType -> I.Term -> I.Term
mkForAll typ term = foldl' f term freeVars
  where
    freeVars = collectFreeVars typ
    f curTerm typeVar = I.TypeForall (I.PatternVar typeVar) (I.ScopedTerm curTerm)
