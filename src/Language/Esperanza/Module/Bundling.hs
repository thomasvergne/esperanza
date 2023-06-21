module Language.Esperanza.Module.Bundling where

import qualified Control.Monad.Except                      as E
import qualified Control.Monad.State                       as ST
import qualified Data.Map                                  as M
import qualified Data.Text                                 as L
import qualified Language.Esperanza.CST.Expression         as C
import qualified Language.Esperanza.CST.Modules.Annotated  as C
import qualified Language.Esperanza.CST.Modules.Located    as C
import qualified Language.Esperanza.CST.Modules.Namespaced as D
import qualified Language.Esperanza.CST.Modules.Type       as C
import qualified Language.Esperanza.CST.Toplevel           as C
import qualified Language.Esperanza.Module.AST             as A
import qualified Language.Esperanza.Module.Monad           as M

-- | Create a name from a module name and with a variable name
createNameFromModule :: M.MonadBundling m => Text -> m Text
createNameFromModule name = do
  paths <- ST.gets M.currentModule
  return $
    (if not (null paths)
       then L.intercalate "." paths <> "."
       else "") <>
    name

-- | Create a name from a namespaced name
makeNameFromNamespaced :: D.Namespaced -> Text
makeNameFromNamespaced (D.Namespaced paths name) =
  (if not (null paths)
     then L.intercalate "." paths <> "."
     else "") <>
  name
makeNameFromNamespaced (D.Simple name) = name

-- | Restrain the current state from outside influences
-- | and do not merge the state after the computation
local ::
     M.MonadBundling m
  => (M.MonadBundlingState -> M.MonadBundlingState)
  -> m a
  -> m a
local f m = do
  s <- ST.get
  ST.modify f
  a <- m
  ST.put s
  return a

insertName :: M.MonadBundling m => Text -> Text -> m ()
insertName name value = do
  s <- ST.get
  ST.put $ s {M.variableMapping = M.insert name value (M.variableMapping s)}

insertType :: M.MonadBundling m => Text -> Text -> m ()
insertType name value = do
  s <- ST.get
  ST.put $ s {M.typeMapping = M.insert name value (M.typeMapping s)}

bundleToplevel ::
     M.MonadBundling m => C.Located C.Toplevel -> m [C.Located A.Toplevel]
bundleToplevel (C.Located pos (C.TData (C.Annotated name gens) fields)) = do
  name' <- createNameFromModule name
  insertType name name'
  fields' <- mapM (mapM bundleType) fields
  return [C.Located pos $ A.TData (C.Annotated name' gens) fields']
bundleToplevel (C.Located pos (C.TDeclaration (C.Annotated name ty) expr)) = do
  name' <- createNameFromModule name
  insertName name name'
  ty' <- bundleMaybeType ty
  expr' <- bundleExpression expr
  return [C.Located pos $ A.TDeclaration (C.Annotated name' ty') expr']
bundleToplevel (C.Located pos (C.TExtern (C.Annotated name ty))) = do
  name' <- createNameFromModule name
  ty' <- bundleMaybeType ty
  return [C.Located pos $ A.TExtern (C.Annotated name' ty')]
bundleToplevel (C.Located pos (C.TFunction (C.Annotated name ty) args expr)) = do
  name' <- createNameFromModule name
  insertName name name'
  ty' <- bundleMaybeType ty
  args' <- mapM (\(C.Annotated e t) -> C.Annotated e <$> bundleMaybeType t) args
  expr' <- bundleExpression expr
  return [C.Located pos $ A.TFunction (C.Annotated name' ty') args' expr']
bundleToplevel (C.Located pos (C.TImport _)) =
  E.throwError (M.ShouldNotHappen "TImport", pos)
bundleToplevel (C.Located _ (C.TModule name tls)) = do
  ST.modify $ \s -> s {M.currentModule = M.currentModule s <> [name]}
  tls' <- concat <$> mapM bundleToplevel tls
  ST.modify $ \s ->
    s {M.currentModule = fromMaybe [] (viaNonEmpty init (M.currentModule s))}
  return tls'

bundleMaybeType :: M.MonadBundling m => C.Ty -> m (Maybe (A.Type A.Expression))
bundleMaybeType Nothing   = return Nothing
bundleMaybeType (Just ty) = Just <$> bundleType ty

bundleType ::
     M.MonadBundling m => C.Type C.Expression -> m (A.Type A.Expression)
bundleType (C.TypeId name) = do
  let name' = makeNameFromNamespaced name
  tys <- ST.gets M.typeMapping
  return $
    case M.lookup name' tys of
      Nothing -> A.TypeId name'
      Just ty -> A.TypeId ty
bundleType (C.TypeApp ty arg) = do
  ty' <- bundleType ty
  arg' <- bundleType arg
  return $ A.TypeApp ty' arg'
bundleType (C.TypeRefinement (name, ty) expr) = do
  ty' <- bundleType ty
  expr' <-
    case expr of
      Nothing    -> return Nothing
      Just expr' -> Just <$> bundleExpression expr'
  return $ A.TypeRefinement (name, ty') expr'
bundleType (C.TypeVar name) = return $ A.TypeVar name
bundleType C.TypeInt = return A.TypeInt
bundleType C.TypeBool = return A.TypeBool
bundleType C.TypeVoid = return A.TypeVoid
bundleType C.TypeChar = return A.TypeChar
bundleType C.TypeFloat = return A.TypeFloat

bundleExpression ::
     M.MonadBundling m => C.Located C.Expression -> m (C.Located A.Expression)
bundleExpression (C.Located p (C.ELiteral l)) =
  return $ C.Located p $ A.ELiteral l
bundleExpression (C.Located p (C.EVariable ns)) = do
  let ns' = makeNameFromNamespaced ns
  vars <- ST.gets M.variableMapping
  return $
    case M.lookup ns' vars of
      Nothing -> C.Located p $ A.EVariable ns'
      Just v  -> C.Located p $ A.EVariable v
bundleExpression (C.Located p (C.EDataType ns)) = do
  let ns' = makeNameFromNamespaced ns
  vars <- ST.gets M.variableMapping
  return $
    case M.lookup ns' vars of
      Nothing -> C.Located p $ A.EDataType ns'
      Just v  -> C.Located p $ A.EDataType v
bundleExpression (C.Located p (C.EApplication f arg)) = do
  f' <- bundleExpression f
  arg' <- bundleExpression arg
  return $ C.Located p $ A.EApplication f' arg'
bundleExpression (C.Located p (C.EIf cond then' else')) = do
  cond' <- bundleExpression cond
  then'' <- bundleExpression then'
  else'' <- bundleExpression else'
  return $ C.Located p $ A.EIf cond' then'' else''
bundleExpression (C.Located p (C.ECase expr cases)) = do
  expr' <- bundleExpression expr
  cases' <-
    mapM (\(pat, br) -> (,) <$> bundlePattern pat <*> bundleExpression br) cases
  return $ C.Located p $ A.ECase expr' cases'
bundleExpression (C.Located p (C.EFunction args expr)) = do
  args' <- mapM (\(C.Annotated n t) -> C.Annotated n <$> bundleMaybeType t) args
  expr' <- bundleExpression expr
  return $ C.Located p $ A.EFunction args' expr'
bundleExpression (C.Located p (C.ELet mappings expr)) = do
  mappings' <-
    mapM
      (\(C.Annotated n t, e) ->
         (,) <$> (C.Annotated n <$> bundleMaybeType t) <*> bundleExpression e)
      (M.toList mappings)
  expr' <-
    case expr of
      Nothing    -> return Nothing
      Just expr' -> Just <$> bundleExpression expr'
  return $ C.Located p $ A.ELet (M.fromList mappings') expr'
bundleExpression (C.Located p (C.EBlock expr)) = do
  expr' <- mapM bundleExpression expr
  return $ C.Located p $ A.EBlock expr'
bundleExpression (C.Located p (C.EList exprs)) = do
  exprs' <- mapM bundleExpression exprs
  return $ C.Located p $ A.EList exprs'
bundleExpression (C.Located p (C.EListAccess expr index)) = do
  expr' <- bundleExpression expr
  index' <- bundleExpression index
  return $ C.Located p $ A.EListAccess expr' index'

bundlePattern ::
     M.MonadBundling m => C.Located C.CaseBranch -> m (C.Located A.CaseBranch)
bundlePattern (C.Located p (C.PLiteral l)) = return $ C.Located p $ A.PLiteral l
bundlePattern (C.Located p (C.PVariable ns)) =
  return $ C.Located p $ A.PVariable ns
bundlePattern (C.Located p (C.PConstructor ns args)) = do
  let ns' = makeNameFromNamespaced ns
  args' <- mapM bundlePattern args
  return $ C.Located p $ A.PConstructor ns' args'
bundlePattern (C.Located p C.PHole) = return $ C.Located p A.PHole
