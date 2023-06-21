{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Esperanza.Parser.Expression where

import qualified Control.Applicative                         as A
import           Data.Char                                   (isUpper)
import           Data.List                                   (foldl)
import qualified Data.Map                                    as M
import qualified Language.Esperanza.CST.Expression           as C
import qualified Language.Esperanza.CST.Modules.Annotated    as C
import qualified Language.Esperanza.CST.Modules.Located      as C
import qualified Language.Esperanza.CST.Modules.Type         as D
import qualified Language.Esperanza.Parser.Lexer             as L
import           Language.Esperanza.Parser.Modules.Literal   (parseNamespaced)
import qualified Language.Esperanza.Parser.Modules.Literal   as L
import           Language.Esperanza.Parser.Modules.Operators (makeUnaryOp,
                                                              operatorTable)
import           Language.Esperanza.Parser.Modules.Pattern   (parsePattern)
import qualified Text.Parsec                                 as P
import qualified Text.Parsec.Expr                            as E

parseExpression :: L.Esperanza C.Expression
parseExpression = do
  let table = operatorTable
  E.buildExpressionParser (table' ++ table) parseTerm
  where
    table' = [[E.Postfix $ makeUnaryOp postfix]]
    postfix = listAccess
    listAccess = do
      index <- L.brackets parseExpression
      e <- P.getPosition
      return $ \x@(C.Located (s, _) _) -> C.EListAccess x index C.:>: (s, e)

parseTerm :: L.Esperanza C.Expression
parseTerm =
  P.choice
    [ L.parens parseExpression
    , parseBlock
    , parseApp
    , parseVariable
    , parseIf
    , parseFunction
    , parseLet
    , parseCase
    , parseLiteral
    , parseList
    ]

parseLiteral :: L.Esperanza C.Expression
parseLiteral = L.lexeme $ C.ELiteral <$> L.parseLiteral

parseVariable :: L.Esperanza C.Expression
parseVariable =
  L.lexeme $ do
    name <- parseNamespaced
    let n = toString $ L.getVariable name
    case n of
      (x:_)
        | isUpper x -> return $ C.EDataType name
      _ -> return $ C.EVariable name

parseIf :: L.Esperanza C.Expression
parseIf =
  L.lexeme $ do
    L.reserved "if"
    cond <- parseExpression
    then' <- L.indented (L.reserved "then" *> parseExpression)
    else' <- L.indented (L.reserved "else" *> parseExpression)
    return $ C.EIf cond then' else'

parseBlock :: L.Esperanza C.Expression
parseBlock =
  L.locate $ do
    L.reservedOp "do"
    C.EBlock <$> L.block parseExpression

parseCallee :: L.Esperanza C.Expression
parseCallee =
  P.choice [parseVariable, L.parens parseExpression, parseLiteral, parseList]

parseApp :: L.Esperanza C.Expression
parseApp = do
  callee <- parseCallee
  args <- many (L.guardIndent *> parseCallee)
  return $
    foldl
      (\x@(C.Located (s, _) _) y@(C.Located (_, e) _) ->
         C.EApplication x y C.:>: (s, e))
      callee
      args

parseLet :: L.Esperanza C.Expression
parseLet =
  L.lexeme $ do
    L.reserved "let"
    name <-
      M.fromList <$>
      L.block
        ((,) <$>
         (C.Annotated <$> L.identifier <*>
          P.optionMaybe (L.reservedOp ":" *> parseType)) <*>
         (L.reservedOp "=" *> parseExpression))
    C.ELet name <$> P.optionMaybe (L.reserved "in" *> parseExpression)

parseCase :: L.Esperanza C.Expression
parseCase =
  L.lexeme $ do
    L.reserved "match"
    value <- parseExpression
    L.reserved "with"
    P.optional (L.reservedOp "|")
    C.ECase value <$>
      L.block
        ((,) <$> parsePattern <*>
         L.indented (L.reservedOp "=>" *> parseExpression))

parseFunction :: L.Esperanza C.Expression
parseFunction =
  L.lexeme $ do
    L.reservedOp "fun"
    args <-
      A.some
        (L.parens
           (do name <- L.identifier
               ty <-
                 P.optionMaybe
                   (do L.reservedOp ":"
                       parseType)
               return (C.Annotated name ty)) <|>
         C.Annotated <$> L.identifier <*> pure Nothing)
    L.reservedOp "in"
    C.EFunction args <$> L.indented parseExpression

parseList :: L.Esperanza C.Expression
parseList =
  L.lexeme $ C.EList <$> L.brackets (L.commaSep (L.indented parseExpression))

parseType :: L.Parser (D.Type C.Expression)
parseType = E.buildExpressionParser table term
  where
    table =
      [ [E.Infix (return D.TypeApp) E.AssocLeft]
      , [ E.Infix
            (L.reservedOp "->" >> return (D.TypeApp . D.TypeApp (D.TypeId "->")))
            E.AssocRight
        ]
      , [ E.Infix
            (L.operator >>= \op ->
               return (D.TypeApp . D.TypeApp (D.TypeId (fromString op))))
            E.AssocRight
        ]
      ]

refinement :: L.Parser (D.Type C.Expression)
refinement =
  L.parens $ do
    name <- L.lowered
    L.reservedOp ":"
    ty <- parseType
    expr <- P.optionMaybe (L.reservedOp "|" *> parseExpression)
    return $ D.TypeRefinement (fromString name, ty) expr

term :: L.Parser (D.Type C.Expression)
term =
  P.choice
    [ L.reserved "float" $> D.TypeFloat <|> L.reserved "int" $> D.TypeInt
    , L.reserved "string" $> D.TypeApp (D.TypeId "[]") D.TypeChar
    , L.reserved "char" $> D.TypeChar
    , L.reserved "unit" $> D.TypeVoid
    , L.brackets (D.TypeApp (D.TypeId "[]") <$> parseType)
    , refinement
    , P.try (fromString <$> L.lowered <&> D.TypeVar)
    , (L.namespacedUpper <|> fromString <$> L.parens L.operator) <&> D.TypeId
    , L.parens parseType
    ]
