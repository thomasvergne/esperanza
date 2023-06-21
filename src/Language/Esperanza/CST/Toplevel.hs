module Language.Esperanza.CST.Toplevel where

import qualified Data.Map                                 as M
import           Language.Esperanza.CST.Expression        (Expression, Ty)
import           Language.Esperanza.CST.Modules.Annotated (Annotated)
import           Language.Esperanza.CST.Modules.Located   (Located)
import           Language.Esperanza.CST.Modules.Type
import           Prelude                                  hiding (Type)
import qualified Language.Esperanza.CST.Modules.Annotated as C

data Toplevel
  = TModule Text [Located Toplevel]
  | TImport [Text]
  -- ^ Module related
  | TData (Annotated [Text]) (M.Map Text [Type Expression])
  -- ^ Datatypes related
  | TDeclaration (C.Annotated Ty) (Located Expression)
  | TFunction (C.Annotated Ty) [Annotated Ty] (Located Expression)
  -- ^ Variable definitions related
  | TExtern (C.Annotated Ty)
  deriving (Eq, Show)
