module Main where

import           Language.Esperanza.AST.Conversion      (convertToplevel)
import qualified Language.Esperanza.CST.Modules.Located as C
import qualified Language.Esperanza.CST.Toplevel        as T
import           Language.Esperanza.Module              (runModuleBundlingPass,
                                                         runModuleResolvingPass)
import qualified Language.Esperanza.Parser              as P

main :: IO ()
main = do
  let file = "examples/main.esp"
  x <- decodeUtf8 <$> readFileBS file
  let result = P.parseEsperanza file x
  case result of
    Right (C.Located _ (T.TModule "Main" toplevels)) -> do
      resolved <- runModuleResolvingPass "examples" toplevels
      case resolved of
        Right ast' -> do
          bundled <- runModuleBundlingPass ast'
          case bundled of
            Right ast'' -> do
              let ast''' = map convertToplevel ast''
              print ast'''
            Left err -> print err
        Left err -> print err
    Left err -> print err
    _ -> print ("Invalid file" :: String)
