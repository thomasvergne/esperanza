module Language.Esperanza.AST.AST where

import qualified Data.Map                                 as M
import           Language.Esperanza.CST.Modules.Annotated (Annotated)
import qualified Language.Esperanza.CST.Modules.Annotated as C
import           Language.Esperanza.CST.Modules.Located   (Located)
import qualified Language.Esperanza.Module.AST            as A
import           Prelude                                  hiding (Type)

data Toplevel
  = TData (Annotated [Text]) (M.Map Text [A.Type A.Expression])
  -- ^ Datatypes related
  | TDeclaration (C.Annotated A.Ty) (Located A.Expression)
  -- ^ Variable definitions related
  | TExtern (C.Annotated A.Ty)
  deriving (Eq, Show)
