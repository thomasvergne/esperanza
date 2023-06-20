module Language.Esperanza.CST.Expression where

import qualified Data.Map                                  as M
import           Language.Esperanza.CST.Modules.Literal    (Literal)
import           Language.Esperanza.CST.Modules.Namespaced (Namespaced)
import           Language.Esperanza.CST.Modules.Located    (Located)
import Language.Esperanza.CST.Modules.Type ( Type )
import Language.Esperanza.CST.Modules.Annotated ( Annotated )
import           Prelude                                   hiding (Type)

type Ty = Maybe (Type Expression)

data Expression
  = EVariable Namespaced
  | EDataType Namespaced
  -- ^ Variable related
  | EApplication (Located Expression) (Located Expression)
  -- ^ Call related
  | EIf (Located Expression) (Located Expression) (Located Expression)
  | ELet (M.Map (Annotated Ty) (Located Expression)) (Maybe (Located Expression))
  | ECase (Located Expression) [(Located CaseBranch, Located Expression)]
  | EFunction [Annotated Ty] (Located Expression)
  | ELiteral Literal
  | EBlock [Located Expression]
  -- ^ Misc related
  | EList [Located Expression]
  | EListAccess (Located Expression) (Located Expression)
  -- ^ List related
  deriving (Eq, Show, Ord)

data CaseBranch
  = PLiteral Literal
  | PVariable Text
  | PRecord Namespaced (M.Map (Located CaseBranch) Text)
  | PConstructor Namespaced [Located CaseBranch]
  | PHole
  deriving (Eq, Show, Ord)
