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

convertVarId :: F.VarIdent -> I.VarIdent
convertVarId (F.VarIdent varId) = I.VarIdent varId

convertVarIdToPattern :: F.VarIdent -> I.Pattern
convertVarIdToPattern (F.VarIdent varId) = I.PatternVar $ I.VarIdent varId

convertFuncAppArg :: F.FuncAppArg -> I.Term
convertFuncAppArg = \case
  F.FuncAppArgInt n -> I.ConstInt n
  F.FuncAppArgVar varId -> I.Var (convertVarId varId)

convertDecl :: F.Decl -> Either ConvertError (I.ScopedTerm -> I.Term)
convertDecl decl = case decl of
  F.AnnotatedDecl (F.Annotation annId typ) (F.PlainDecl varId varValue)
    | annId /= varId -> Left $
      DifferentNameForBindingAndAnnotationError annId varId decl
    | otherwise -> do
      convertedVarValue <- convert varValue
      pure $ \letBody ->
        I.Let (convertVarIdToPattern varId) (I.Ann (convertRType typ) convertedVarValue) letBody
  F.UnAnnotatedDecl (F.PlainDecl varId varValue) ->  do
      convertedVarValue <- convert varValue
      pure $ \letBody ->
        I.Let (convertVarIdToPattern varId) convertedVarValue  letBody

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

convertBaseType :: F.BaseType -> I.BaseType
convertBaseType = \case
  F.BaseTypeInt -> I.BaseTypeInt

convertPredicate :: F.Pred -> I.Term
convertPredicate predicate = case predicate of
  F.PVar varId -> I.Var (convertVarId varId)
  F.PTrue -> I.ConstTrue
  F.PFalse -> I.ConstFalse
  F.PInt n -> I.ConstInt n
  F.PEq l r -> I.OpExpr (convertPredicate l) I.EqOp (convertPredicate r)
  F.PLessThan l r ->  I.OpExpr (convertPredicate l) I.LessOp (convertPredicate r)
  F.PLessOrEqThan l r ->  I.OpExpr (convertPredicate l) I.LessOrEqOp (convertPredicate r)
  F.PPlus l r ->  I.OpExpr (convertPredicate l) I.PlusOp (convertPredicate r)
  F.PMinus l r ->  I.OpExpr (convertPredicate l) I.MinusOp (convertPredicate r)
  F.PMultiply l r ->  I.OpExpr (convertPredicate l) I.MultiplyOp (convertPredicate r)
