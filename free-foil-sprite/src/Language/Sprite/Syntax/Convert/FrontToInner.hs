module Language.Sprite.Syntax.Convert.FrontToInner where

import Language.Sprite.Syntax.Front.Abs qualified as F
import Language.Sprite.Syntax.Inner.Abs qualified as I

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
  F.Let decl (F.ScopedTerm body) -> do
    (I.ScopedTerm -> convertedBody) <- convert body
    let_ <- convertDecl decl
    pure $ let_ convertedBody
  F.Fun argId (F.ScopedTerm body) -> do
    (I.ScopedTerm -> convertedBody) <- convert body
    pure $ I.Fun (convertVarIdToPattern argId) convertedBody
  F.App funcTerm argTerm -> do
    convertedFuncTerm <- convert funcTerm
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
      pure $ \letBody ->
        I.Let (convertVarIdToPattern varId) (I.Ann (convertRType typ) convertedVarValue) letBody
  F.UnAnnotatedDecl varId varValue ->  do
      convertedVarValue <- convert varValue
      pure $ \letBody ->
        I.Let (convertVarIdToPattern varId) convertedVarValue  letBody
  F.RecDecl (F.Annotation annId typ) varId varValue
    | annId /= varId -> Left $
        DifferentNameForBindingAndAnnotationError annId varId decl
    | otherwise -> do
      convertedVarValue <- convert varValue
      pure $ \letBody ->
        I.LetRec (convertRType typ) (convertVarIdToPattern varId) (I.ScopedTerm convertedVarValue) letBody

convertRType :: F.RType -> I.Term
convertRType rType = case rType of
  F.TypeRefined base varId predicate ->
    I.TypeRefined
      (convertBaseType base)
      (convertVarIdToPattern varId)
      (I.ScopedTerm $ convertPredicate predicate)

  F.TypeFun (F.NamedFuncArg argId argType) (F.ScopedRType retType) ->
    I.TypeFun
      (convertVarIdToPattern argId)
      (convertRType argType)
      (I.ScopedTerm $ convertRType retType)

  F.TypeRefinedUnknown base -> I.TypeRefinedUnknown (convertBaseType base)

convertBaseType :: F.BaseType -> I.BaseType
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
