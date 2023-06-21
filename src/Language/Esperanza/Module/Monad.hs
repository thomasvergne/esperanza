module Language.Esperanza.Module.Monad where

import qualified Control.Monad.Except                   as E
import qualified Control.Monad.State                    as ST
import qualified Data.Map                               as M
import qualified Language.Esperanza.CST.Modules.Located as C

type MonadBundling m
   = ( E.MonadError (MonadBundlingError, C.Position) m
     , ST.MonadState MonadBundlingState m)

data MonadBundlingState =
  MonadBundlingState
    { currentModule   :: [Text]
    , variableMapping :: M.Map Text Text
    , typeMapping     :: M.Map Text Text
    }
  deriving (Show)

newtype MonadBundlingError =
  ShouldNotHappen Text
  deriving (Show)
