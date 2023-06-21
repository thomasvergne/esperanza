{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Esperanza.Module.Resolving where

import qualified Control.Monad.Except                   as E
import qualified Control.Monad.State                    as ST
import qualified Data.Map                               as M
import qualified Language.Esperanza.CST.Modules.Located as C
import qualified Language.Esperanza.CST.Toplevel        as C
import qualified Language.Esperanza.Parser              as P
import qualified System.Directory                       as D
import qualified System.FilePath                        as FP
import qualified Text.Parsec                            as P

type MonadResolver m
   = ( MonadIO m
     , E.MonadError (ModuleResolutionError, C.Position) m
     , ST.MonadState ModuleGraph m)

data ModuleResolutionError
  = ModuleNotFound Text
  | ModuleNotParsed Text P.ParseError
  | ModuleInvalid Text
  | ModuleCycle Text Text
  deriving (Show, Eq)

data Module =
  Module
    { modulePath      :: Text
    , moduleToplevels :: [C.Located C.Toplevel]
    }
  deriving (Show, Eq)

data ModuleGraph =
  ModuleGraph
    { moduleGraphModules :: M.Map Text Module
    , moduleGraphImports :: M.Map Text [Text]
    , currentModule      :: Maybe [Text]
    , currentDirectory   :: Text
    }
  deriving (Show, Eq)

resolveOpen :: MonadResolver m => Text -> C.Position -> m Module
resolveOpen path pos = do
  graph <- ST.get
  let path' =
        toString (currentDirectory graph) FP.</> toString path FP.-<.> ".esp"
  let imports = fromMaybe [] $ M.lookup path (moduleGraphImports graph)
  when (path `elem` imports) $ E.throwError (ModuleCycle path path, pos)
  fileExists <- liftIO $ D.doesFileExist path'
  if fileExists
    then do
      content <- liftIO $ decodeUtf8 <$> readFileBS path'
      let ast = P.parseEsperanza path' content
      case ast of
        Left err -> E.throwError (ModuleNotParsed (fromString path') err, pos)
        Right (C.Located _ (C.TModule n ast')) -> do
          let imports' = readImports ast'
          tls <- resolveToplevels ast'
          let module' = Module n tls
          ST.modify
            (\graph' ->
               graph'
                 { moduleGraphModules =
                     M.insert path module' (moduleGraphModules graph')
                 , moduleGraphImports =
                     M.insert path imports' (moduleGraphImports graph')
                 })
          pure module'
        _ -> E.throwError (ModuleInvalid (fromString path'), pos)
    else E.throwError (ModuleNotFound path, pos)

readImports :: [C.Located C.Toplevel] -> [Text]
readImports (C.Located _ (C.TImport path):xs) = path : readImports xs
readImports (_:xs)                            = readImports xs
readImports _                                 = []

resolveToplevels ::
     MonadResolver m => [C.Located C.Toplevel] -> m [C.Located C.Toplevel]
resolveToplevels (C.Located pos (C.TModule name toplevels):xs) = do
  tls <- resolveToplevels toplevels
  (C.Located pos (C.TModule name tls) :) <$> resolveToplevels xs
resolveToplevels (C.Located pos (C.TImport path):xs) = do
  Module n tls <- resolveOpen path pos
  (C.Located pos (C.TModule n tls) :) <$> resolveToplevels xs
resolveToplevels (x:xs) = (x :) <$> resolveToplevels xs
resolveToplevels [] = pure []
