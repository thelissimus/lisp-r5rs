{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Lib (module Lib) where

import Control.Applicative (liftA2)
import Control.Monad.Except (MonadError (catchError, throwError))
import Data.Array (Array, elems, listArray)
import Data.Complex (Complex ((:+)))
import Data.Functor (($>), (<&>))
import Data.Kind (Type)
import Data.List (foldl1')
import Data.Ratio (denominator, numerator, (%))
import Numeric (readBin, readDec, readHex, readOct)

import Text.ParserCombinators.Parsec hiding (spaces)

eval :: LispVal -> Either LispError LispVal
eval = \case
  v@(String _) -> pure v
  v@(Number _) -> pure v
  v@(Bool _) -> pure v
  (List [Atom "if", cond, conseq, alt]) ->
    eval cond >>= \case
      Bool True -> eval conseq
      Bool False -> eval alt
      other -> throwError $ TypeMismatch "boolean" other
  (List [Atom "quote", v]) -> pure v
  (List (Atom f : args)) -> mapM eval args >>= apply f
  other -> throwError $ BadSpecialForm "Unrecognized special form" other

apply :: String -> [LispVal] -> Either LispError LispVal
apply f args =
  maybe
    (throwError . NotAFunction "Unrecognized primitive function args" $ f)
    ($ args)
    (lookup f primitives)

primitives :: [(String, [LispVal] -> Either LispError LispVal)]
primitives =
  [ ("+", liftBinNumeric (+))
  , ("-", liftBinNumeric (-))
  , ("*", liftBinNumeric (*))
  , ("/", liftBinNumeric div)
  , ("mod", liftBinNumeric mod)
  , ("quotient", liftBinNumeric quot)
  , ("remainder", liftBinNumeric rem)
  , ("not", liftUnary notP)
  , ("boolean?", liftUnary booleanP)
  , ("list?", liftUnary listP)
  , ("symbol?", liftUnary symbolP)
  , ("number?", liftUnary numberP)
  , ("char?", liftUnary charP)
  , ("string?", liftUnary stringP)
  , ("vector?", liftUnary vectorP)
  , ("symbol->string", liftUnary symbolToStringP)
  , ("string->symbol", liftUnary stringToSymbolP)
  , ("=", liftBinBool unpackNumber (==))
  , ("/=", liftBinBool unpackNumber (/=))
  , ("<", liftBinBool unpackNumber (<))
  , (">", liftBinBool unpackNumber (>))
  , ("<=", liftBinBool unpackNumber (<=))
  , (">=", liftBinBool unpackNumber (>=))
  , ("&&", liftBinBool unpackBool (&&))
  , ("||", liftBinBool unpackBool (||))
  , ("string=?", liftBinBool unpackStr (==))
  , ("string<?", liftBinBool unpackStr (<))
  , ("string>?", liftBinBool unpackStr (>))
  , ("string<=?", liftBinBool unpackStr (<=))
  , ("string>=?", liftBinBool unpackStr (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)
  ]

liftBinNumeric :: (Integer -> Integer -> Integer) -> [LispVal] -> Either LispError LispVal
liftBinNumeric _ [] = throwError $ ArgsArity 2 []
liftBinNumeric _ a@[_] = throwError $ ArgsArity 2 a
liftBinNumeric f as = Number . foldl1' f <$> mapM unpackNumber as

liftUnary :: (LispVal -> LispVal) -> [LispVal] -> Either LispError LispVal
liftUnary f [a] = pure . f $ a
liftUnary _ xs = throwError $ ArgsArity 1 xs

liftBinBool :: (LispVal -> Either LispError a) -> (a -> a -> Bool) -> [LispVal] -> Either LispError LispVal
liftBinBool unpack f [a, b] = Bool <$> liftA2 f (unpack a) (unpack b)
liftBinBool _ _ xs = throwError $ ArgsArity 2 xs

unpackVal :: String -> (LispVal -> Maybe a) -> LispVal -> Either LispError a
unpackVal t f v = case f v of
  Just a -> pure a
  Nothing -> throwError $ TypeMismatch t v

unpackNumber :: LispVal -> Either LispError Integer
unpackNumber = unpackVal "number" $ \case Number n -> Just n; _ -> Nothing

unpackBool :: LispVal -> Either LispError Bool
unpackBool = unpackVal "boolean" $ \case Bool b -> Just b; _ -> Nothing

unpackStr :: LispVal -> Either LispError String
unpackStr = unpackVal "string" $ \case
  String s -> Just s
  Number n -> Just (show n)
  _ -> Nothing

notP :: LispVal -> LispVal
notP (Bool x) = Bool . not $ x
notP _ = Bool False

booleanP :: LispVal -> LispVal
booleanP (Bool _) = Bool True
booleanP _ = Bool False

listP :: LispVal -> LispVal
listP (List _) = Bool True
listP (DottedList _ _) = Bool True
listP _ = Bool False

symbolP :: LispVal -> LispVal
symbolP (Atom _) = Bool True
symbolP _ = Bool False

numberP :: LispVal -> LispVal
numberP (Number _) = Bool True
numberP _ = Bool False

charP :: LispVal -> LispVal
charP (Char _) = Bool True
charP _ = Bool False

stringP :: LispVal -> LispVal
stringP (String _) = Bool True
stringP _ = Bool False

vectorP :: LispVal -> LispVal
vectorP (Vector _) = Bool True
vectorP _ = Bool False

symbolToStringP :: LispVal -> LispVal
symbolToStringP (Atom s) = String s

stringToSymbolP :: LispVal -> LispVal
stringToSymbolP (String s) = Atom s

car :: [LispVal] -> Either LispError LispVal
car = \case
  [List (x : _)] -> pure x
  [DottedList (x : _) _] -> pure x
  [other] -> throwError $ TypeMismatch "pair" other -- pair???
  other -> throwError $ ArgsArity 1 other

cdr :: [LispVal] -> Either LispError LispVal
cdr = \case
  [List (_ : xs)] -> pure $ List xs
  [DottedList [_] t] -> pure t
  [DottedList (_ : xs) t] -> pure $ DottedList xs t
  [other] -> throwError $ TypeMismatch "pair" other
  other -> throwError $ ArgsArity 1 other

cons :: [LispVal] -> Either LispError LispVal
cons = \case
  [x, List []] -> pure $ List [x]
  [x, List xs] -> pure $ List (x : xs)
  [x, DottedList xs t] -> pure $ DottedList (x : xs) t
  [x, y] -> pure $ DottedList [x] y
  other -> throwError $ ArgsArity 2 other

eqv :: [LispVal] -> Either LispError LispVal
eqv = \case
  [Bool a, Bool b] -> pure $ Bool (a == b)
  [Number a, Number b] -> pure $ Bool (a == b)
  [String a, String b] -> pure $ Bool (a == b)
  [Atom a, Atom b] -> pure $ Bool (a == b)
  [DottedList xs x, DottedList ys y] -> eqv [List (xs ++ [x]), List (ys ++ [y])]
  [List xs, List ys] -> pure . Bool $ (length xs == length ys) && all eqvPair (zip xs ys)
  [_, _] -> pure $ Bool False
  other -> throwError $ ArgsArity 2 other
 where
  eqvPair :: (LispVal, LispVal) -> Bool -- todo: error handling
  eqvPair (a, b) = case eqv [a, b] of
    Left _ -> False
    Right (Bool val) -> val

equal :: [LispVal] -> Either LispError LispVal
equal [a, b] = do
  primRes <-
    or
      <$> mapM
        (unpackEquals a b)
        [ AnyUnpacker unpackNumber
        , AnyUnpacker unpackBool
        , AnyUnpacker unpackStr
        ]
  eqvRes <- eqv [a, b]
  pure $ Bool (primRes || let (Bool x) = eqvRes in x)
equal other = throwError $ ArgsArity 2 other

readExpr :: String -> Either LispError LispVal
readExpr s = case parse parseExpr "lisp" s of
  Left err -> throwError . ParsingError $ err
  Right val -> pure val

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

type LispVal :: Type
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
    (DottedList xs t) -> "(" ++ unwordsList xs ++ " . " ++ show t ++ ")"
    (Vector xs) -> "#(" ++ unwordsList (elems xs) ++ ")"

type LispError :: Type
data LispError
  = ArgsArity Integer [LispVal]
  | TypeMismatch String LispVal
  | ParsingError ParseError
  | BadSpecialForm String LispVal
  | NotAFunction String String
  deriving stock (Eq)

instance Show LispError where
  show = \case
    (ArgsArity n vals) -> "Expected " ++ show n ++ " args; found values " ++ unwordsList vals
    (TypeMismatch t val) -> "Invalid type: expected " ++ t ++ ", found " ++ show val
    (ParsingError e) -> "Parse error at " ++ show e
    (BadSpecialForm msg form) -> msg ++ ": " ++ show form
    (NotAFunction msg f) -> msg ++ ": " ++ f

type Unpacker :: Type
data Unpacker = forall a. (Eq a) => AnyUnpacker (LispVal -> Either LispError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> Either LispError Bool
unpackEquals a b (AnyUnpacker u) = liftA2 (==) (u a) (u b) `catchError` const (pure False)

trapError :: (MonadError e m, Show e) => m String -> m String
trapError = flip catchError (pure . show)

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
  choice . map charMapF $ charsEscapeMap ++ charsWhiteSpaceMap

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
