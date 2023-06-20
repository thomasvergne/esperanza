module Language.Esperanza.CST.Modules.Namespaced where
import qualified Text.Show as T
import qualified Data.Text as L

data Namespaced
  = Simple Text
  | Namespaced [Text] Text
  deriving (Eq, Ord)

instance T.Show Namespaced where
  show (Simple n) = show n
  show (Namespaced ns n) = toString $ L.intercalate "::" (ns ++ [n])

instance IsString Namespaced where
  fromString = Simple . fromString