module Language.Esperanza.Module where

import qualified Control.Monad.Except                   as E
import qualified Control.Monad.State                    as ST
import qualified Language.Esperanza.CST.Modules.Located as C
import qualified Language.Esperanza.CST.Toplevel        as C
import qualified Language.Esperanza.Module.AST          as A
import qualified Language.Esperanza.Module.Bundling     as M
import qualified Language.Esperanza.Module.Monad        as M
import qualified Language.Esperanza.Module.Resolving    as MR

runModuleResolvingPass ::
     MonadIO m
  => Text
  -> [C.Located C.Toplevel]
  -> m (Either (MR.ModuleResolutionError, C.Position) [C.Located C.Toplevel])
runModuleResolvingPass dir xs =
  E.runExceptT $
  ST.evalStateT
    (MR.resolveToplevels xs)
    (MR.ModuleGraph mempty mempty mempty dir)

runModuleBundlingPass ::
     Monad m
  => [C.Located C.Toplevel]
  -> m (Either (M.MonadBundlingError, C.Position) [C.Located A.Toplevel])
runModuleBundlingPass xs =
  E.runExceptT $
  ST.evalStateT
    (concat <$> mapM M.bundleToplevel xs)
    (M.MonadBundlingState mempty mempty mempty)
