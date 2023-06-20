{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Language.Esperanza.Parser.Toplevel where

import qualified Control.Applicative                      as A
import qualified Data.Map                                 as M
import qualified Language.Esperanza.CST.Modules.Annotated as C
import           Language.Esperanza.CST.Modules.Located   (Located (unLoc))
import qualified Language.Esperanza.CST.Modules.Type      as TP
import qualified Language.Esperanza.CST.Toplevel          as T
import qualified Language.Esperanza.Parser.Expression     as PE
import qualified Language.Esperanza.Parser.Lexer          as L
import qualified Text.Parsec                              as P

parseVariableDeclaration :: L.Esperanza T.Toplevel
parseVariableDeclaration =
  L.lexeme $ do
    L.reserved "let"
    name <- L.identifier
    args <-
      A.many $
      L.parens
        (do arg <- L.identifier
            ty <-
              P.optionMaybe $ do
                L.reservedOp ":"
                ty <- PE.parseType
                ref <-
                  P.optionMaybe
                    (L.reservedOp "|" *> (unLoc <$> PE.parseExpression))
                case ref of
                  Nothing -> return ty
                  Just _  -> return (TP.TypeRefinement (arg, ty) ref)
            return (C.Annotated arg ty)) <|>
      C.Annotated <$> L.identifier <*> pure Nothing
    ty <-
      P.optionMaybe
        (do L.reservedOp ":"
            PE.parseType)
    L.reservedOp "="
    body <- PE.parseExpression
    let name' = C.Annotated name ty
    return $
      if null args
        then T.TDeclaration name' body
        else T.TFunction name' args body

parseDataDeclaration :: L.Esperanza T.Toplevel
parseDataDeclaration =
  L.lexeme $ do
    L.reserved "type"
    name <- L.identifier
    generics <- P.many L.identifier
    x <- L.indented (L.reservedOp "=" *> parseCons)
    xs <- many (L.indented (L.reservedOp "|" *> parseCons))
    return $ T.TData (C.Annotated name generics) (M.fromList $ x : xs)
  where
    parseCons = do
      name' <- L.identifier
      args <- P.many PE.parseType
      return (name', args)

parseModuleDefinition :: L.Esperanza T.Toplevel
parseModuleDefinition =
  L.lexeme $ do
    L.reserved "module"
    name <- L.identifier
    xs <- L.block parseToplevel
    return $ T.TModule name xs

parseImport :: L.Esperanza T.Toplevel
parseImport =
  L.lexeme $ do
    L.reserved "open"
    names <-
      P.sepBy1
        (L.identifier <|> fromString <$> L.parens L.operator)
        (L.reservedOp "::")
    return $ T.TImport names

parseToplevel :: L.Esperanza T.Toplevel
parseToplevel =
  P.choice [parseImport, parseDataDeclaration, parseVariableDeclaration]
