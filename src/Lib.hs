{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}

module Lib (module Lib) where

import Data.Array (Array, elems, listArray)
import Data.Complex (Complex ((:+)))
import Data.Functor (($>), (<&>))
import Data.List (foldl1')
import Data.Ratio (Rational, denominator, numerator, (%))
import Numeric (readBin, readDec, readHex, readOct)

import Text.ParserCombinators.Parsec hiding (spaces)

eval :: LispVal -> LispVal
eval = \case
  v@(String _) -> v
  v@(Number _) -> v
  v@(Bool _) -> v
  (List [Atom "quote", v]) -> v
  (List (Atom f : args)) -> apply f . map eval $ args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) . lookup f $ primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinF (+))
  , ("-", numericBinF (-))
  , ("*", numericBinF (*))
  , ("/", numericBinF div)
  , ("mod", numericBinF mod)
  , ("quotient", numericBinF quot)
  , ("remainder", numericBinF rem)
  ]

numericBinF :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinF f = Number . foldl1' f . map unpackNum

unpackNum :: LispVal -> Integer
unpackNum = \case
  (Number n) -> n
  (String n) ->
    let parsed = reads n :: [(Integer, String)]
     in if null parsed
          then 0
          else fst . head $ parsed
  (List [n]) -> unpackNum n
  _ -> 0

readExpr :: String -> LispVal
readExpr s = case parse parseExpr "lisp" s of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> try parseChar
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseRatio
    <|> try parseNumber
    <|> try parseBool
    <|> parseQuoted
    <|> parseQuasiquote
    <|> try parseUnquoteSplicing
    <|> parseUnquote
    <|> parseVector
    <|> parseList

data LispVal
  = Atom String
  | Bool Bool
  | Number Integer
  | Ratio Rational
  | Float Double
  | Complex (Complex Double)
  | Char Char
  | String String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Array Int LispVal)
  deriving stock (Eq)

instance Show LispVal where
  show = \case
    (Atom iden) -> iden
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    (Number n) -> show n
    (Ratio n) -> show (numerator n) ++ "/" ++ show (denominator n)
    (Float n) -> show n
    (Complex (r :+ i)) -> show r ++ "+" ++ show i ++ "i"
    (Char c) -> show c
    (String s) -> show s
    (List xs) -> "(" ++ unwordsList xs ++ ")"
    (DottedList head tail) -> "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
    (Vector xs) -> "#(" ++ unwordsList (elems xs) ++ ")"
   where
    unwordsList :: [LispVal] -> String
    unwordsList = unwords . map show

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  pure $ Atom (first : rest)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  oneOf "tf" <&> \case
    't' -> Bool True
    'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber = parseNumberPlain <|> parseNumberRadix

parseNumberPlain :: Parser LispVal
parseNumberPlain = Number . read <$> many1 digit

parseNumberRadix :: Parser LispVal
parseNumberRadix = do
  char '#'
  parseNumberBinary <|> parseNumberOctal <|> parseNumberDecimal <|> parseNumberHexadecimal

parseNumberBinary :: Parser LispVal
parseNumberBinary = do
  char 'b'
  Number . (fst . head) . readBin <$> many (oneOf "01")

parseNumberOctal :: Parser LispVal
parseNumberOctal = do
  char 'o'
  Number . (fst . head) . readOct <$> many (oneOf "012344567")

parseNumberDecimal :: Parser LispVal
parseNumberDecimal = do
  char 'd'
  Number . (fst . head) . readDec <$> many (oneOf "0123456789")

parseNumberHexadecimal :: Parser LispVal
parseNumberHexadecimal = do
  char 'x'
  Number . (fst . head) . readHex <$> many (oneOf "01234567890abcdefABCDEF")

parseRatio :: Parser LispVal
parseRatio = do
  n <- read <$> many1 digit
  char '/'
  d <- read <$> many1 digit
  pure $ Ratio (n % d)

parseFloat :: Parser LispVal
parseFloat = do
  w <- many1 digit
  char '.'
  d <- many1 digit
  pure $ Float (read (w ++ "." ++ d))

parseComplex :: Parser LispVal
parseComplex = do
  r <- toDouble <$> (try parseFloat <|> parseNumberPlain)
  char '+'
  i <- toDouble <$> (try parseFloat <|> parseNumberPlain)
  char 'i'
  pure $ Complex (r :+ i)
 where
  toDouble (Float n) = n
  toDouble (Number n) = fromIntegral n

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  many1 letter <&> \case
    "space" -> Char ' '
    "newline" -> Char '\n'
    [c] -> Char c

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escaped <|> noneOf (map fst charsEscapeMap))
  char '"'
  pure (String x)

parseList :: Parser LispVal
parseList = char '(' >> (parseEmpty <|> parsePopulated)
 where
  parseEmpty :: Parser LispVal
  parseEmpty = char ')' $> List []

  parsePopulated :: Parser LispVal
  parsePopulated = parseExpr >>= decide . pure

  decide :: [LispVal] -> Parser LispVal
  decide expr = plainList expr <|> (spaces >> dottedList expr)

  plainList :: [LispVal] -> Parser LispVal
  plainList expr = char ')' $> List (reverse expr)

  dottedList :: [LispVal] -> Parser LispVal
  dottedList expr =
    do
      char '.' >> spaces
      dotted <- parseExpr
      char ')'
      pure $ DottedList expr dotted
      <|> (parseExpr >>= \next -> decide (next : expr))

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  expr <- parseExpr
  pure $ List [Atom "quote", expr]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  char '`'
  expr <- parseExpr
  pure $ List [Atom "quasiquote", expr]

parseUnquote :: Parser LispVal
parseUnquote = do
  char ','
  expr <- parseExpr
  pure $ List [Atom "unquote", expr]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  string ",@"
  expr <- parseExpr
  pure $ List [Atom "unquote-splicing", expr]

parseVector :: Parser LispVal
parseVector = do
  string "#("
  vec <- sepBy parseExpr spaces
  char ')'
  pure $ Vector (listArray (1, length vec) vec)

charsEscapeMap :: [(Char, Char)]
charsEscapeMap = [('\\', '\\'), ('"', '"')]

charsWhiteSpaceMap :: [(Char, Char)]
charsWhiteSpaceMap = [('n', '\n'), ('r', '\r'), ('t', '\t')]

charMapF :: (Char, Char) -> Parser Char
charMapF (c, r) = char c $> r

escaped :: Parser Char
escaped = do
  char '\\'
  choice . map charMapF $ (charsEscapeMap ++ charsWhiteSpaceMap)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space
