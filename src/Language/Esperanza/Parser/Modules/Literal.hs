{-# LANGUAGE LambdaCase #-}

module Language.Esperanza.Parser.Modules.Literal where

import           Data.Char                                 (isUpper)
import qualified Language.Esperanza.CST.Modules.Literal    as C
import qualified Language.Esperanza.CST.Modules.Namespaced as D
import qualified Language.Esperanza.Parser.Lexer           as L
import qualified Text.Parsec                               as P

parseLiteral :: L.Parser C.Literal
parseLiteral =
  P.choice
    [ P.try $ C.Float <$> L.float
    , C.Int <$> L.integer
    , C.Char <$> L.charLiteral
    , C.String <$> L.stringLiteral
    ]

parseNamespaced :: L.Parser D.Namespaced
parseNamespaced =
  P.try
    (do names <- L.identifier `P.sepBy1` P.char '.'
        case names of
          [name] -> return $ D.Simple name
          _:_ -> do
            let x =
                  viaNonEmpty init names >>= \x' ->
                    viaNonEmpty last names >>= \y -> return (y, x')
            case x of
              Just (n, ns) -> return $ D.Namespaced ns n
              Nothing      -> fail "Impossible"
          _ -> fail "Impossible") P.<|>
  D.Simple <$>
  L.identifier

namespacedUpper :: L.Parser D.Namespaced
namespacedUpper =
  parseNamespaced >>= \case
    D.Simple n ->
      case viaNonEmpty head (toString n) of
        Nothing -> fail "Empty namespace"
        Just n' ->
          if isUpper n'
            then return $ D.Simple n
            else fail "Not a type"
    D.Namespaced ns n ->
      case viaNonEmpty head (toString n) of
        Nothing -> fail "Empty namespace"
        Just n' ->
          if isUpper n'
            then return $ D.Namespaced ns n
            else fail "Not a type"

getVariable :: D.Namespaced -> Text
getVariable (D.Simple n)       = n
getVariable (D.Namespaced _ n) = n