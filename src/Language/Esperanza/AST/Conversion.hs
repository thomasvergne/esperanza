module Language.Esperanza.AST.Conversion where

import qualified Language.Esperanza.AST.AST               as AST
import qualified Language.Esperanza.CST.Modules.Annotated as C
import qualified Language.Esperanza.CST.Modules.Located   as C
import qualified Language.Esperanza.Module.AST            as A

convertToplevel :: C.Located A.Toplevel -> C.Located AST.Toplevel
convertToplevel (C.Located pos (A.TFunction (C.Annotated name _) args body)) = do
  C.Located pos $
    AST.TDeclaration
      (C.Annotated name Nothing)
      (C.Located pos $ A.EFunction args body)
convertToplevel (C.Located pos (A.TDeclaration (C.Annotated name _) body)) = do
  C.Located pos $ AST.TDeclaration (C.Annotated name Nothing) body
convertToplevel (C.Located pos (A.TExtern (C.Annotated name _))) = do
  C.Located pos $ AST.TExtern (C.Annotated name Nothing)
convertToplevel (C.Located pos (A.TData (C.Annotated name gens) constructors)) = do
  C.Located pos $ AST.TData (C.Annotated name gens) constructors
