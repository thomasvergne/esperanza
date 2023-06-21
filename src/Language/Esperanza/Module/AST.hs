module Language.Esperanza.Module.AST where

import qualified Data.Map                                 as M
import           Language.Esperanza.CST.Modules.Annotated (Annotated)
import qualified Language.Esperanza.CST.Modules.Annotated as C
import qualified Language.Esperanza.CST.Modules.Literal   as L
import           Language.Esperanza.CST.Modules.Located   (Located)
import           Prelude                                  hiding (Type)

type Ty = Maybe (Type Expression)

data Toplevel
  = TData (Annotated [Text]) (M.Map Text [Type Expression])
  -- ^ Datatypes related
  | TFunction (C.Annotated Ty) [Annotated Ty] (Located Expression)
  | TDeclaration (C.Annotated Ty) (Located Expression)
  -- ^ Variable definitions related
  | TExtern (C.Annotated Ty)
  deriving (Eq, Show)

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
  | TypeId Text
  -- ^ TypeVar denotes a type identifier, e.g. T
  --   The argument is the name of the type variable
  --   For example, T is represented as
  --   TypeVar "T"
  | TypeVar Text
  -- ^ TypeVar denotes a type variable, e.g. a
  --   The argument is the name of the type variable
  --   For example, a is represented as
  --   TypeVar "a"
  | TypeRefinement (Text, Type a) (Maybe (Located a))
  -- ^ TypeRefinement denotes a type refinement, e.g. (x: Int | x > 0)
  --   The first argument is the name of the refinement variable
  --   The second argument is the type of the refinement variable
  --   The third argument is the refinement predicate
  --   For example, (x: Int | x > 0) is represented as
  --   TypeRefinement ("x", TypeInt) (Just (x > 0))
  deriving (Eq, Show, Ord)

data Expression
  = EVariable Text
  | EDataType Text
  -- ^ Variable related
  | EApplication (Located Expression) (Located Expression)
  -- ^ Call related
  | EIf (Located Expression) (Located Expression) (Located Expression)
  | ELet
      (M.Map (Annotated Ty) (Located Expression))
      (Maybe (Located Expression))
  | ECase (Located Expression) [(Located CaseBranch, Located Expression)]
  | EFunction [Annotated Ty] (Located Expression)
  | ELiteral L.Literal
  | EBlock [Located Expression]
  -- ^ Misc related
  | EList [Located Expression]
  | EListAccess (Located Expression) (Located Expression)
  -- ^ List related
  deriving (Eq, Show, Ord)

data CaseBranch
  = PLiteral L.Literal
  | PVariable Text
  | PConstructor Text [Located CaseBranch]
  | PHole
  deriving (Eq, Show, Ord)
