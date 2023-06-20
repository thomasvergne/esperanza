module Language.Esperanza.Parser where

import qualified Language.Esperanza.CST.Modules.Located as C
import qualified Language.Esperanza.CST.Toplevel        as C
import qualified Language.Esperanza.Parser.Lexer        as L
import qualified Language.Esperanza.Parser.Toplevel     as T
import qualified Text.Parsec as P
import qualified Control.Monad.Reader as R

type SourceFile = String

parseEsperanza ::
     SourceFile
  -> Text
  -> (Either P.ParseError (C.Located C.Toplevel))
parseEsperanza file content =
  R.runReader
    (P.runParserT
       (L.whiteSpace *> T.parseModuleDefinition <* P.eof)
       ()
       file
       content)
    (L.Indent 1 1)