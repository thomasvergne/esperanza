{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Esperanza.Parser.Lexer where

import qualified Control.Monad.Reader                   as R
import           Data.Char
import           Language.Esperanza.CST.Modules.Located
import qualified Text.Parsec                            as P
import qualified Text.Parsec.Token                      as Token

type Esperanza a = Parser (Located a)

type Parser a = P.ParsecT Text () (R.ReaderT Indentation Identity) a

data Indentation =
  Indent Int Int

block :: Parser a -> Parser [a]
block p = do
  pos <- P.getPosition
  (Indent n line) <- R.ask
  -- Checking for same line indentation
  if P.sourceLine pos == line
    then many p
    else if P.sourceColumn pos > n && P.sourceLine pos > line
  -- Setting indent state to new source column value
           then many
                  (guardIndent *>
                   R.local
                     (const $ Indent (P.sourceColumn pos) (P.sourceLine pos))
                     p)
           else fail $ "indentation of " <> show (n + 1) <> " spaces needed, but got " <>
                show (P.sourceColumn pos)

indented :: Parser a -> Parser a
indented p = do
  pos <- P.getPosition
  (Indent n line) <- R.ask
  if (P.sourceColumn pos > n && P.sourceLine pos > line) ||
     (P.sourceLine pos == line)
    then R.local (const $ Indent (P.sourceColumn pos) (P.sourceLine pos)) p
    else fail $ "indentation of " <> show (n + 1) <> " spaces needed, but got " <>
                show (P.sourceColumn pos - 1)

guardIndent :: Parser ()
guardIndent = do
  pos <- P.getPosition
  (Indent n _) <- R.ask
  if P.sourceColumn pos > n
    then return ()
    else fail $ "indentation of " <> show (n + 1) <> " spaces needed, but got " <>
                show (P.sourceColumn pos - 1)

reservedWords :: [String]
reservedWords =
  [ "let"
  , "in"
  , "if"
  , "then"
  , "else"
  , "match"
  , "with"
  , "module"
  , "open"
  , "do"
  , "fun"
  , "type"
  ]

languageDef :: Token.GenLanguageDef Text u (R.ReaderT Indentation Identity)
languageDef =
  Token.LanguageDef
    { Token.commentStart = "(*"
    , Token.commentEnd = "*)"
    , Token.commentLine = ""
    , Token.nestedComments = True
    , Token.caseSensitive = True
    , Token.identStart = P.letter <|> P.char '_'
    , Token.opLetter = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Token.opStart = Token.opLetter languageDef
    , Token.identLetter = P.alphaNum <|> P.char '_' <|> P.char '\''
    , Token.reservedNames = reservedWords
    , Token.reservedOpNames =
        [ "("
        , ")"
        , "*"
        , "+"
        , "-"
        , "/"
        , "{"
        , "}"
        , "["
        , "]"
        , "<"
        , ">"
        , "="
        , "->"
        , "|"
        ]
    }

lexer :: Token.GenTokenParser Text u (R.ReaderT Indentation Identity)
lexer = Token.makeTokenParser languageDef

identifier :: Parser Text
identifier = fromString <$> Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

comma :: Parser String
comma = Token.comma lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semi :: Parser String
semi = Token.semi lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

locate :: Parser a -> Esperanza a
locate p = do
  start <- P.getPosition
  r <- p
  end <- P.getPosition
  return (r :>: (start, end))

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

decimal :: Parser Integer
decimal = Token.decimal lexer

lexeme :: Parser a -> Parser (Located a)
lexeme p = do
  p1 <- P.getPosition
  x <- p
  p2 <- P.getPosition
  whiteSpace
  return $ x :>: (p1, p2)

lexeme' :: Parser a -> Parser a
lexeme' p = do
  x <- p
  whiteSpace
  return x

parseEither ::
     Esperanza a -> Esperanza b -> Parser (Either (Located a) (Located b))
parseEither pa pb = Left <$> P.try pa <|> Right <$> pb

lowered :: Parser String
lowered =
  identifier >>=
  (\(x:xs) ->
     if isLower x
       then return (x : xs)
       else fail "not lowercase") .
  toString

capitalized :: Parser String
capitalized =
  lexeme'
    (do c <- P.upper
        cs <- many P.alphaNum
        return (c : cs)) >>= \x -> guard (x `notElem` reservedWords) >> return x

characterChar :: Parser Char
characterChar = charLetter <|> charEscape

operator :: Parser String
operator = Token.operator lexer

charEscape :: Parser Char
charEscape = do
  _ <- P.char '\\'
  escapeCode

charLetter :: Parser Char
charLetter = P.satisfy (\c -> c /= '\"' && c /= '{' && c /= '\\' && c > '\026')

escapeCode :: Parser Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <|> charLetter

charControl :: Parser Char
charControl = do
  _ <- P.char '^'
  code <- P.upper
  return (toEnum (fromEnum code - fromEnum 'A' + 1))

charNum :: Parser Char
charNum = do
  code <-
    decimal <|> do
      _ <- P.char 'o'
      number 8 P.octDigit <|> do
        _ <- P.char 'x'
        number 16 P.hexDigit
  if code > 0x10FFFF
    then fail "invalid escape sequence"
    else return (toEnum (fromInteger code))

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit = do
  digits <- P.many1 baseDigit
  let n = foldl' (\x d -> base * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

charEsc :: Parser Char
charEsc = P.choice (map parseEsc escMap)
  where
    parseEsc (c, code) = do
      _ <- P.char c
      return code

charAscii :: Parser Char
charAscii = P.choice (map parseAscii asciiMap)
  where
    parseAscii (asc, code) =
      P.try
        (do _ <- P.string asc
            return code)

escMap :: [(Char, Char)]
escMap = zip "{abf{rtv\\\"\'" "{\a\b\f\n\r\t\v\\\"\'"

asciiMap :: [(String, Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes =
  [ "BS"
  , "HT"
  , "LF"
  , "VT"
  , "FF"
  , "CR"
  , "SO"
  , "SI"
  , "EM"
  , "FS"
  , "GS"
  , "RS"
  , "US"
  , "SP"
  ]

ascii3codes :: [String]
ascii3codes =
  [ "NUL"
  , "SOH"
  , "STX"
  , "ETX"
  , "EOT"
  , "ENQ"
  , "ACK"
  , "BEL"
  , "DLE"
  , "DC1"
  , "DC2"
  , "DC3"
  , "DC4"
  , "NAK"
  , "SYN"
  , "ETB"
  , "CAN"
  , "SUB"
  , "ESC"
  , "DEL"
  ]

ascii2 :: String
ascii2 =
  [ '\BS'
  , '\HT'
  , '\LF'
  , '\VT'
  , '\FF'
  , '\CR'
  , '\SO'
  , '\SI'
  , '\EM'
  , '\FS'
  , '\GS'
  , '\RS'
  , '\US'
  , '\SP'
  ]

ascii3 :: String
ascii3 =
  [ '\NUL'
  , '\SOH'
  , '\STX'
  , '\ETX'
  , '\EOT'
  , '\ENQ'
  , '\ACK'
  , '\BEL'
  , '\DLE'
  , '\DC1'
  , '\DC2'
  , '\DC3'
  , '\DC4'
  , '\NAK'
  , '\SYN'
  , '\ETB'
  , '\CAN'
  , '\SUB'
  , '\ESC'
  , '\DEL'
  ]
