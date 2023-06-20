module Main where
import qualified Language.Esperanza.Parser as P
import qualified Language.Esperanza.CST.Toplevel as C
import qualified Language.Esperanza.CST.Modules.Located as C

main :: IO ()
main = do
  let file = "examples/main.esp"
  x <- decodeUtf8 <$> readFileBS file
  let ast = P.parseEsperanza file x
  case ast of
    Right ast -> print ast
    Left err -> print err