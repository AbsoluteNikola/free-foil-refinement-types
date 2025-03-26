module Language.Sprite.Syntax.Convert.QualifierToFTR where
import qualified Language.Fixpoint.Types as T
import qualified Language.Sprite.Syntax.Front.Abs as F
import qualified Language.Sprite.Syntax.Convert.FrontToInner as F
import qualified Language.Sprite.Syntax.Convert.InnerToFTR as I
import Data.Functor ((<&>))
import qualified Language.Fixpoint.Types.Names as FST

convertQualifier :: FilePath -> F.Qualifier -> Either I.ConvertError T.Qualifier
convertQualifier filePath (F.Qualifier (F.VarIdent name) args p) = do
  body <- I.convertTerm $ F.convertPredicate p
  pure $ T.Q
    { qName = T.symbol name
    , qParams = qualArgs
    , qBody = body
    , qPos = T.dummyPos filePath
    }
  where
    qualArgs = args <&> \(F.QualifierArg (F.VarIdent varId) base) -> T.QP
        { qpSym = T.symbol varId
        , qpPat = T.PatNone
        , qpSort = baseTypeToSort base
        }

baseTypeToSort :: F.BaseType ->  T.Sort
baseTypeToSort = \case
  F.BaseTypeInt -> T.intSort
  F.BaseTypeBool -> T.boolSort
  F.BaseTypeVar (F.TypeVarId (F.VarIdent v)) -> T.FObj . FST.symbol $ v
