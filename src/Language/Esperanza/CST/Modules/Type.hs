module Language.Esperanza.CST.Modules.Type where

import           Prelude hiding (Type)
import Language.Esperanza.CST.Modules.Namespaced (Namespaced)

data Type a
  = TypeInt
  | TypeBool
  | TypeChar
  | TypeVoid
  | TypeFloat
  -- ^ TypeInt denotes the type Int
  --   TypeBool denotes the type Bool
  --   TypeChar denotes the type Char
  --   TypeString denotes the type String
  --   TypeVoid denotes the type ()
  --   TypeFloat denotes the type Float
  | TypeApp (Type a) (Type a)
  -- ^ TypeApp denotes a type application, e.g. User Int
  --   The first argument is the name of the type constructor
  --   The second argument is a list of type arguments
  --   For example, List Int is represented as
  --   TypeApp "List" [TypeInt]
  | TypeId Namespaced
  -- ^ TypeVar denotes a type identifier, e.g. T
  --   The argument is the name of the type variable
  --   For example, T is represented as
  --   TypeVar "T"
  | TypeVar Text
  -- ^ TypeVar denotes a type variable, e.g. a
  --   The argument is the name of the type variable
  --   For example, a is represented as
  --   TypeVar "a"
  | TypeRefinement (Text, Type a) (Maybe a)
  -- ^ TypeRefinement denotes a type refinement, e.g. (x: Int | x > 0)
  --   The first argument is the name of the refinement variable
  --   The second argument is the type of the refinement variable
  --   The third argument is the refinement predicate
  --   For example, (x: Int | x > 0) is represented as
  --   TypeRefinement ("x", TypeInt) (Just (x > 0))
  | TypeFunction [Type a] (Type a)
  -- ^ TypeFunction denotes a function type, e.g. Int -> Int
  --   The first argument is a list of argument types
  --   The second argument is the return type
  deriving (Eq, Show, Ord)

data Class a
  = Class Namespaced [Type a]
  deriving (Eq, Show)

data TypeScheme a
  = TypeForall [Class a] (Type a)
  deriving (Eq, Show)