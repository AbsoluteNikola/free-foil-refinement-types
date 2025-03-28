module Language.Sprite.TypeCheck.Constraints where

import Language.Sprite.Syntax.Inner.Abs qualified as Inner
import Language.Sprite.Syntax.Convert.InnerToFTR qualified as InnerToFTR
import Language.Fixpoint.Horn.Types qualified as H
import Language.Fixpoint.Types.Sorts qualified as T
import Data.String (fromString)
import Data.Text (Text)
import qualified Language.Sprite.Syntax.Inner.Abs as I
import qualified Language.Fixpoint.Types as FT
import Language.Sprite.TypeCheck.Monad (showT, CheckerM, debugPrintT, getRawVarIdFromPattern, getNameBinderFromPattern)
import qualified Control.Monad.Foil as F
import Language.Sprite.Syntax
import qualified Control.Monad.Free.Foil as F
import qualified Language.Fixpoint.Types.Sorts as FTS
import Control.Monad.Error.Class (throwError)


data Constraint
  = CPred Inner.Term Text
  | CAnd [Constraint]
  | CImplication Inner.VarIdent FTS.Sort Inner.Term Constraint Text
  deriving (Show)

cTrue :: Constraint
cTrue = CAnd []

baseTypeToSort :: I.Term -> Either Text T.Sort
baseTypeToSort = \case
  I.BaseTypeInt -> pure T.intSort
  I.BaseTypeBool -> pure T.boolSort
  I.BaseTypeVar (I.Var (I.VarIdent v))-> pure . T.FObj . FT.symbol $ v
  term -> Left $ "baseTypeToSort called on: " <> showT term

buildImplicationFromType :: (F.DExt i o) => Text -> F.Scope o -> Pattern i o -> Term o -> Constraint -> CheckerM Constraint
buildImplicationFromType msg scope argVarPat typ constraint =
  sortPred scope argVarPat typ >>= \case
    Just (typSort, p) -> do
      let argVarIdRaw = getRawVarIdFromPattern argVarPat
      pure $ CImplication argVarIdRaw typSort (fromTerm p) constraint msg
    Nothing -> pure constraint
  -- case typ of
  -- TypeRefined base (PatternVar typVarId) p -> do
  --   let
  --     subst = F.addRename (F.sink F.identitySubst) typVarId (F.nameOf argVarId)
  --     p' = F.substitute scope subst p
  --     argVarIdRaw = getRawVarIdFromPattern argVarPat
  --     Inner.VarIdent argVarIdRawName = argVarIdRaw

  --   debugPrintT $ "Implication: " <> "varId = " <> showT argVarIdRawName <> ", term = " <> showT p'
  --   pure $ CImplication argVarIdRaw (fromTerm base) (fromTerm p') constraint msg
  -- _ -> pure constraint

sortPred :: F.DExt i o => F.Scope o -> Pattern i o -> Term o -> CheckerM (Maybe (FTS.Sort, Term o))
sortPred scope (PatternVar argVarId) typ = case typ of
  TypeRefined _ (PatternVar typVarId) p -> do
    let
      subst = F.addRename (F.sink F.identitySubst) typVarId (F.nameOf argVarId)
      p' = F.substitute scope subst p
    typSort <- case getTypeSort typ of
      Right sort -> pure sort
      Left err -> throwError err
    pure $ Just (typSort, p')
  TypeData _ _ (PatternVar typVarId) p -> do
    let
      subst = F.addRename (F.sink F.identitySubst) typVarId (F.nameOf argVarId)
      p' = F.substitute scope subst p
    typSort <- case getTypeSort typ of
      Right sort -> pure sort
      Left err -> throwError err
    pure $ Just (typSort, p')
  _ -> pure Nothing

getTypeSort :: Term i -> Either Text FTS.Sort
getTypeSort = \case
  TypeRefined b _ _ -> baseTypeToSort (fromTerm b)
  TypeData (Inner.VarIdent typName) typArgs _ _ -> do
    argsSorts <- traverse getTypeSort typArgs
    pure $ FTS.fAppTC
      (FTS.symbolFTycon . FT.dummyLoc  . FT.symbol $ typName)
      argsSorts
  TypeFun _ argTyp retTyp -> do
    argSort <- getTypeSort argTyp
    retSort <- getTypeSort retTyp
    pure $ FTS.FFunc argSort retSort
  TypeForall typVarPat typeUnderForAll -> do
    typeUnderForAllSort <- getTypeSort typeUnderForAll
    typVarRawId <- case fromPattern typVarPat of
      (Inner.PatternVar (Inner.VarIdent typVarRawId)) -> Right $ FT.symbol typVarRawId
      pat -> Left $ "Unknown pattern in forall: " <> showT pat
    let
      -- Тут используется хак, что внутреннее представление Foil это Int
      -- и каждый биндер уникальный. Поэтому мы его и используется в fixpoint sort
      typVarIndex = F.nameId . F.nameOf $ getNameBinderFromPattern typVarPat
      subst = FTS.mkSortSubst [(typVarRawId, FTS.FVar typVarIndex)]
    pure $ FTS.FAbs typVarIndex (FTS.sortSubst subst typeUnderForAllSort)
  term -> Left $ "getTypeSort called on: " <> showT term

constraintsToFHT :: Constraint -> Either InnerToFTR.ConvertError (H.Cstr Text)
constraintsToFHT = \case
  CPred p msg -> do
    pred' <- InnerToFTR.convert p
    pure $ H.Head pred' msg
  CAnd cs -> do H.CAnd <$> traverse constraintsToFHT cs
  CImplication (I.VarIdent varId) sort p c msg -> do
    p' <- InnerToFTR.convert p
    c' <- constraintsToFHT c
    pure $ H.All
      (H.Bind
        (fromString varId)
        sort
        p'
        msg)
      c'
