{-# LANGUAGE OverloadedStrings #-}

module Language.Esperanza.Parser.Modules.Operators where

import qualified Control.Applicative                    as A
import qualified Control.Monad.Reader                   as R
import           Data.Foldable                          (Foldable (foldr1))
import qualified Language.Esperanza.CST.Expression      as C
import qualified Language.Esperanza.CST.Modules.Located as C
import           Language.Esperanza.Parser.Lexer        (Indentation)
import qualified Language.Esperanza.Parser.Lexer        as L
import qualified Text.Parsec                            as P
import qualified Text.Parsec.Expr                       as E

operatorTable ::
     [[E.Operator Text () (R.ReaderT Indentation Identity) (C.Located C.Expression)]]
operatorTable =
  [ [propCallBin "||" E.AssocLeft, propCallBin "&&" E.AssocLeft]
  , [propCallBin "!=" E.AssocLeft, propCallBin "==" E.AssocLeft]
  , [ propCallBin ">=" E.AssocLeft
    , propCallBin ">" E.AssocLeft
    , propCallBin "<=" E.AssocLeft
    , propCallBin "<" E.AssocLeft
    ]
  , [propCallBin "+" E.AssocLeft, propCallBin "-" E.AssocLeft]
  , [ propCallBin "%" E.AssocLeft
    , propCallBin "/" E.AssocLeft
    , propCallBin "*" E.AssocLeft
    ]
  , [propCallUn "-", propCallUn "+"]
  , [propCallUn "!", propCallUn "--", propCallUn "++"]
  , [propCallPost "--", propCallPost "++"]
  , [common]
  ]

propCallBin ::
     String
  -> E.Assoc
  -> E.Operator Text () (R.ReaderT Indentation Identity) (C.Located C.Expression)
propCallBin name =
  binary
    name
    (\x@(C.Located pos _) y ->
       C.EApplication
         (C.EApplication (C.EVariable (fromString name) C.:>: pos) x C.:>: pos)
         y)

propCallUn ::
     String
  -> E.Operator Text () (R.ReaderT Indentation Identity) (C.Located C.Expression)
propCallUn name =
  prefix
    name
    (\x@(C.Located pos _) ->
       C.EApplication (C.EVariable (fromString name) C.:>: pos) x)

propCallPost ::
     String
  -> E.Operator Text () (R.ReaderT Indentation Identity) (C.Located C.Expression)
propCallPost name =
  postfix
    name
    (\x@(C.Located pos _) ->
       C.EApplication (C.EVariable (fromString name) C.:>: pos) x)

common ::
     E.Operator Text () (R.ReaderT Indentation Identity) (C.Located C.Expression)
common =
  E.Infix
    (do op <- L.operator
        return $ \x@(C.Located (s, _) _) y@(C.Located (_, e) _) ->
          C.EApplication
            (C.EApplication (C.EVariable (fromString op) C.:>: (s, s)) x C.:>:
             (s, s))
            y C.:>:
          (s, e))
    E.AssocLeft

binary ::
     String
  -> (C.Located C.Expression -> C.Located C.Expression -> C.Expression)
  -> E.Assoc
  -> E.Operator Text () (R.ReaderT Indentation Identity) (C.Located C.Expression)
binary name f =
  E.Infix
    (do _ <- L.reservedOp name
        return $ \x@(C.Located (s, _) _) y@(C.Located (_, e) _) ->
          f x y C.:>: (s, e))

prefix ::
     String
  -> (C.Located C.Expression -> C.Expression)
  -> E.Operator Text () (R.ReaderT Indentation Identity) (C.Located C.Expression)
prefix name f =
  E.Prefix
    (do s <- P.getPosition
        _ <- L.reservedOp name
        return $ \x@(C.Located (_, e) _) -> f x C.:>: (s, e))

postfix ::
     String
  -> (C.Located C.Expression -> C.Expression)
  -> E.Operator Text () (R.ReaderT Indentation Identity) (C.Located C.Expression)
postfix name f =
  E.Postfix
    (do _ <- L.reservedOp name
        e <- P.getPosition
        return $ \x@(C.Located (s, _) _) -> f x C.:>: (s, e))

makeUnaryOp :: A.Alternative f => f (a -> a) -> f (a -> a)
makeUnaryOp s = foldr1 (.) . reverse <$> A.some s
