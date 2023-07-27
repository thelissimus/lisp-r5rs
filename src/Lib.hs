{-# LANGUAGE DerivingStrategies #-}

module Lib (module Lib) where

import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = getLine >>= putStrLn . readExpr

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving stock (Show, Eq)

charsEscapeMap :: [(Char, Char)]
charsEscapeMap = [('\\', '\\'), ('"', '"')]

charsWhiteSpaceMap :: [(Char, Char)]
charsWhiteSpaceMap = [('n', '\n'), ('r', '\r'), ('t', '\t')]

charMapF :: (Char, Char) -> Parser Char
charMapF (c, r) = char c >> pure r

escaped :: Parser Char
escaped = do
  char '\\'
  choice . map charMapF $ (charsEscapeMap ++ charsWhiteSpaceMap)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  pure $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escaped <|> noneOf (map fst charsEscapeMap))
  char '"'
  pure (String x)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseNumber <|> parseString

readExpr :: String -> String
readExpr s = case parse parseExpr "lisp" s of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val
