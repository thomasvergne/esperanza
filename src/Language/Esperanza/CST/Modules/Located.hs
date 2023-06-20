{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Esperanza.CST.Modules.Located where

import qualified Text.Show       as T
import Text.Parsec (SourcePos)

type Position = (SourcePos, SourcePos)

data Located a =
  Located
    { loc   :: Position
    , unLoc :: a
    }
  deriving Ord

instance Eq a => Eq (Located a) where
  Located _ x == Located _ y = x == y

pattern (:>:) :: a -> Position -> Located a

pattern a :>: pos = Located pos a

instance T.Show a => T.Show (Located a) where
  show (Located _ x) = show x
