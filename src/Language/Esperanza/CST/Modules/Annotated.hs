{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}

module Language.Esperanza.CST.Modules.Annotated where

import qualified Text.Show as T

-- Representing a binding that binds a type representation
-- to a name.
data Annotated a =
  Annotated
    { annotatedName :: Text
    , annotatedType :: a
    }
  deriving (Eq, Ord, Functor)

pattern (:@) :: Text -> a -> Annotated a

pattern x :@ t = Annotated x t

instance T.Show a => T.Show (Annotated a) where
  show (x :@ t) = show x ++ " : " ++ show t
  show _        = "COMPILER ERROR: Annotated.show"

instance {-# OVERLAPS #-} T.Show (Annotated String) where
  show (x :@ t) = show x ++ " : " ++ t
  show _        = "COMPILER ERROR: Annotated.show"

instance {-# OVERLAPS #-} T.Show a => T.Show (Annotated (Maybe a)) where
  show (x :@ (Just t)) = show x ++ " : " ++ show t
  show (x :@ Nothing)  = show x
  show _               = "COMPILER ERROR: Annotated.show"
