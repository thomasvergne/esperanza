module Language.Esperanza.Parser.Modules.Pattern where

import qualified Control.Applicative                       as A
import qualified Language.Esperanza.CST.Expression         as C
import qualified Language.Esperanza.CST.Expression         as P
import qualified Language.Esperanza.Parser.Lexer           as L
import           Language.Esperanza.Parser.Modules.Literal (namespacedUpper)
import qualified Language.Esperanza.Parser.Modules.Literal as L
import qualified Text.Parsec                               as P

parsePattern :: L.Esperanza C.CaseBranch
parsePattern =
  P.choice
    [ L.parens parsePattern
    , parseLiteral
    , parseHole
    , P.try parseVariable
    , parseConstructor
    ]

parseLiteral :: L.Esperanza C.CaseBranch
parseLiteral = L.lexeme $ P.PLiteral <$> L.parseLiteral

parseVariable :: L.Esperanza C.CaseBranch
parseVariable = L.lexeme $ P.PVariable . fromString <$> L.lowered

parseHole :: L.Esperanza C.CaseBranch
parseHole = L.lexeme $ P.PHole <$ P.char '_'

parseConstructor :: L.Esperanza C.CaseBranch
parseConstructor =
  L.lexeme $ do
    name <- namespacedUpper
    P.PConstructor name <$> A.many parsePattern
