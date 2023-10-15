{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib) where

import Control.Applicative (liftA2)
import Control.Monad (void)
import Control.Monad.Except (MonadError (catchError, throwError))

import Data.Complex (Complex ((:+)))
import Data.Functor (($>), (<&>))
import Data.Kind (Type)
import Data.List (foldl1')
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)

import Numeric (readBin, readDec, readHex, readOct)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser :: Type -> Type
type Parser = Parsec Void Text

type LispVal :: Type
data LispVal
  = Atom Text
  | Bool Bool
  | Number Integer
  | Ratio Rational
  | Float Double
  | Complex (Complex Double)
  | Char Char
  | String Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Vector LispVal)
  deriving stock (Eq)

instance Show LispVal where
  show = \case
    (Atom iden) -> show iden
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    (Number n) -> show n
    (Ratio n) -> show (numerator n) ++ "/" ++ show (denominator n)
    (Float n) -> show n
    (Complex (r :+ i)) -> show r ++ "+" ++ show i ++ "i"
    (Char c) -> show c
    (String s) -> show s
    (List xs) -> "(" ++ (unwords . map show) xs ++ ")"
    (DottedList xs t) -> "(" ++ (unwords . map show) xs ++ " . " ++ show t ++ ")"
    (Vector xs) -> "#(" ++ (unwords . map show) (V.toList xs) ++ ")"

type LispError :: Type
data LispError
  = ArgsArity Integer [LispVal]
  | TypeMismatch Text LispVal
  | ParsingError (ParseErrorBundle Text Void)
  | BadSpecialForm String LispVal
  | NotAFunction Text Text
  | CondClause -- todo: more detailed error; error for empty else case
  | CaseClause
  deriving stock (Eq)

instance Show LispError where
  show = \case
    (ArgsArity n vals) -> "Expected " ++ show n ++ " args; found values " ++ (unwords . map show) vals
    (TypeMismatch t val) -> "Invalid type: expected " ++ show t ++ ", found " ++ show val
    (ParsingError e) -> "Parse error at " ++ show e
    (BadSpecialForm msg form) -> show msg ++ ": " ++ show form
    (NotAFunction msg f) -> show msg ++ ": " ++ show f
    CondClause -> "Expected at least 1 true cond clause"
    CaseClause -> "Expected at least 1 true case clause"

type Unpacker :: Type
data Unpacker = forall a. (Eq a) => AnyUnpacker (LispVal -> Either LispError a)

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
  (List (Atom "cond" : cs)) -> evalCond cs
  (List (Atom "case" : key : cs)) -> eval key >>= flip evalCase cs
  (List [Atom "quote", v]) -> pure v
  (List (Atom f : args)) -> traverse eval args >>= apply f
  other -> throwError $ BadSpecialForm "Unrecognized special form" other

evalCond :: [LispVal] -> Either LispError LispVal
evalCond = \case
  [List (Atom "else" : alt)] -> case alt of
    [] -> throwError CondClause
    cs -> last <$> traverse eval cs
  (List clause : alts) -> case clause of
    (cond : conseq) ->
      eval cond >>= \case
        Bool False -> evalCond alts
        Bool True -> case conseq of
          [] -> pure $ Bool True
          cs -> last <$> traverse eval cs
        other -> throwError $ TypeMismatch "boolean" other
    [] -> throwError CondClause
  [] -> throwError CondClause
  other -> throwError $ TypeMismatch "cond clauses" (List other)

evalCase :: LispVal -> [LispVal] -> Either LispError LispVal
evalCase key = \case
  [List (Atom "else" : alt)] -> evalLast alt
  (List ((List keys) : conseq) : alts) ->
    if any (lispTrue . (\k -> eqv [key, k])) keys
      then evalLast conseq
      else evalCase key alts
  _ -> throwError CaseClause
 where
  lispTrue :: Either a LispVal -> Bool
  lispTrue = \case
    Right (Bool True) -> True
    _ -> False

evalLast :: [LispVal] -> Either LispError LispVal
evalLast = \case
  [] -> throwError $ ArgsArity 1 []
  xs -> last <$> traverse eval xs

apply :: Text -> [LispVal] -> Either LispError LispVal
apply f args =
  maybe
    (throwError $ NotAFunction "Unrecognized primitive function args" f)
    ($ args)
    (lookup f primitives)

primitives :: [(Text, [LispVal] -> Either LispError LispVal)]
primitives =
  [ "+" ~> liftBinNumeric (+)
  , "-" ~> liftBinNumeric (-)
  , "*" ~> liftBinNumeric (*)
  , "/" ~> liftBinNumeric div
  , "mod" ~> liftBinNumeric mod
  , "quotient" ~> liftBinNumeric quot
  , "remainder" ~> liftBinNumeric rem
  , "not" ~> liftUnary notP
  , "boolean?" ~> liftUnary booleanP
  , "list?" ~> liftUnary listP
  , "symbol?" ~> liftUnary symbolP
  , "number?" ~> liftUnary numberP
  , "char?" ~> liftUnary charP
  , "string?" ~> liftUnary stringP
  , "vector?" ~> liftUnary vectorP
  , "symbol->string" ~> liftUnary symbolToStringP
  , "string->symbol" ~> liftUnary stringToSymbolP
  , "=" ~> liftBinBool unpackNumber (==)
  , "/=" ~> liftBinBool unpackNumber (/=)
  , "<" ~> liftBinBool unpackNumber (<)
  , ">" ~> liftBinBool unpackNumber (>)
  , "<=" ~> liftBinBool unpackNumber (<=)
  , ">=" ~> liftBinBool unpackNumber (>=)
  , "&&" ~> liftBinBool unpackBool (&&)
  , "||" ~> liftBinBool unpackBool (||)
  , "string=?" ~> liftBinBool unpackStr (==)
  , "string<?" ~> liftBinBool unpackStr (<)
  , "string>?" ~> liftBinBool unpackStr (>)
  , "string<=?" ~> liftBinBool unpackStr (<=)
  , "string>=?" ~> liftBinBool unpackStr (>=)
  , "car" ~> car
  , "cdr" ~> cdr
  , "cons" ~> cons
  , "eq?" ~> eqv
  , "eqv?" ~> eqv
  , "equal?" ~> equal
  ]
 where
  (~>) :: a -> b -> (a, b)
  name ~> fn = (name, fn)

liftBinNumeric :: (Integer -> Integer -> Integer) -> [LispVal] -> Either LispError LispVal
liftBinNumeric _ [] = throwError $ ArgsArity 2 []
liftBinNumeric _ a@[_] = throwError $ ArgsArity 2 a
liftBinNumeric f as = Number . foldl1' f <$> traverse unpackNumber as

liftUnary :: (LispVal -> LispVal) -> [LispVal] -> Either LispError LispVal
liftUnary f [a] = pure . f $ a
liftUnary _ xs = throwError $ ArgsArity 1 xs

liftBinBool :: (LispVal -> Either LispError a) -> (a -> a -> Bool) -> [LispVal] -> Either LispError LispVal
liftBinBool unpack f [a, b] = Bool <$> liftA2 f (unpack a) (unpack b)
liftBinBool _ _ xs = throwError $ ArgsArity 2 xs

unpackVal :: Text -> (LispVal -> Maybe a) -> LispVal -> Either LispError a
unpackVal t f v = case f v of
  Just a -> pure a
  Nothing -> throwError $ TypeMismatch t v

unpackNumber :: LispVal -> Either LispError Integer
unpackNumber = unpackVal "number" $ \case Number n -> Just n; _ -> Nothing

unpackBool :: LispVal -> Either LispError Bool
unpackBool = unpackVal "boolean" $ \case Bool b -> Just b; _ -> Nothing

unpackStr :: LispVal -> Either LispError Text
unpackStr = unpackVal "string" $ \case
  String s -> Just s
  Number n -> Just (T.pack . show $ n)
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
symbolToStringP _ = undefined

stringToSymbolP :: LispVal -> LispVal
stringToSymbolP (String s) = Atom s
stringToSymbolP _ = undefined

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
  [List xs, List ys] -> pure . Bool $ (length xs == length ys) && all (byPairs eqv) (zip xs ys)
  [_, _] -> pure $ Bool False
  other -> throwError $ ArgsArity 2 other

equal :: [LispVal] -> Either LispError LispVal
equal = \case
  [DottedList xs x, DottedList ys y] -> equal [List (xs ++ [x]), List (ys ++ [y])]
  [List xs, List ys] -> pure . Bool $ (length xs == length ys) && all (byPairs equal) (zip xs ys)
  [a, b] -> do
    primRes <-
      or
        <$> traverse
          (unpackEquals a b)
          [ AnyUnpacker unpackNumber
          , AnyUnpacker unpackBool
          , AnyUnpacker unpackStr
          ]
    eqvRes <- eqv [a, b]
    pure $ Bool (primRes || let (Bool x) = eqvRes in x)
  other -> throwError $ ArgsArity 2 other

byPairs :: ([a] -> Either LispError LispVal) -> (a, a) -> Bool
byPairs f (a, b) = case f [a, b] of
  Left _ -> False
  Right (Bool val) -> val
  _ -> undefined

readExpr :: Text -> Either LispError LispVal
readExpr s = case parse parseExpr "lisp" s of
  Left err -> throwError $ ParsingError err
  Right val -> pure val

parseExpr :: Parser LispVal
parseExpr =
  choice
    [ parseAtom
    , parseString
    , try parseChar
    , try parseComplex
    , try parseFloat
    , try parseRatio
    , try parseNumber
    , try parseBool
    , parseQuoted
    , parseQuasiquote
    , try parseUnquoteSplicing
    , parseUnquote
    , parseVector
    , parseList
    ]

unpackEquals :: LispVal -> LispVal -> Unpacker -> Either LispError Bool
unpackEquals a b (AnyUnpacker u) = liftA2 (==) (u a) (u b) `catchError` const (pure False)

trapError :: (MonadError e m, Show e) => m String -> m String
trapError = flip catchError (pure . show)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letterChar <|> symbol
  rest <- T.pack <$> many (letterChar <|> digitChar <|> symbol)
  pure $ Atom (T.cons first rest)

parseBool :: Parser LispVal
parseBool = do
  void (char '#')
  oneOf ("tf" :: [Char]) <&> \case
    't' -> Bool True
    'f' -> Bool False
    _ -> undefined

parseNumber :: Parser LispVal
parseNumber = parseNumberPlain <|> parseNumberRadix

parseNumberPlain :: Parser LispVal
parseNumberPlain = Number . read <$> some digitChar

parseNumberRadix :: Parser LispVal
parseNumberRadix = do
  void (char '#')
  parseNumberBinary <|> parseNumberOctal <|> parseNumberDecimal <|> parseNumberHexadecimal

parseNumberBinary :: Parser LispVal
parseNumberBinary = do
  void (char 'b')
  Number . (fst . head) . readBin <$> many (oneOf ("01" :: [Char]))

parseNumberOctal :: Parser LispVal
parseNumberOctal = do
  void (char 'o')
  Number . (fst . head) . readOct <$> many (oneOf ("012344567" :: [Char]))

parseNumberDecimal :: Parser LispVal
parseNumberDecimal = do
  void (char 'd')
  Number . (fst . head) . readDec <$> many (oneOf ("0123456789" :: [Char]))

parseNumberHexadecimal :: Parser LispVal
parseNumberHexadecimal = do
  void (char 'x')
  Number . (fst . head) . readHex <$> many (oneOf ("01234567890abcdefABCDEF" :: [Char]))

parseRatio :: Parser LispVal
parseRatio = do
  n <- read <$> some digitChar
  void (char '/')
  d <- read <$> some digitChar
  pure $ Ratio (n % d)

parseFloat :: Parser LispVal
parseFloat = do
  w <- some digitChar
  void (char '.')
  d <- some digitChar
  pure $ Float (read (w ++ "." ++ d))

parseComplex :: Parser LispVal
parseComplex = do
  r <- toDouble <$> (try parseFloat <|> parseNumberPlain)
  void (char '+')
  i <- toDouble <$> (try parseFloat <|> parseNumberPlain)
  void (char 'i')
  pure $ Complex (r :+ i)
 where
  toDouble (Float n) = n
  toDouble (Number n) = fromIntegral n
  toDouble _ = undefined

parseChar :: Parser LispVal
parseChar = do
  void (string "#\\")
  some letterChar <&> \case
    "space" -> Char ' '
    "newline" -> Char '\n'
    [c] -> Char c
    _ -> undefined

parseString :: Parser LispVal
parseString = do
  void (char '"')
  str <- T.pack <$> many (escaped <|> noneOf (map fst charsEscapeMap))
  void (char '"')
  pure (String str)

parseList :: Parser LispVal
parseList = char '(' >> (parseEmpty <|> parsePopulated)
 where
  parseEmpty :: Parser LispVal
  parseEmpty = char ')' $> List []

  parsePopulated :: Parser LispVal
  parsePopulated = parseExpr >>= decide . pure

  decide :: [LispVal] -> Parser LispVal
  decide expr = plainList expr <|> (sc >> dottedList expr)

  plainList :: [LispVal] -> Parser LispVal
  plainList expr = char ')' $> List (reverse expr)

  dottedList :: [LispVal] -> Parser LispVal
  dottedList expr =
    do
      char '.' >> sc
      dotted <- parseExpr
      void (char ')')
      pure $ DottedList expr dotted
      <|> (parseExpr >>= \next -> decide (next : expr))

parseQuoted :: Parser LispVal
parseQuoted = do
  void (char '\'')
  expr <- parseExpr
  pure $ List [Atom "quote", expr]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  void (char '`')
  expr <- parseExpr
  pure $ List [Atom "quasiquote", expr]

parseUnquote :: Parser LispVal
parseUnquote = do
  void (char ',')
  expr <- parseExpr
  pure $ List [Atom "unquote", expr]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  void (string ",@")
  expr <- parseExpr
  pure $ List [Atom "unquote-splicing", expr]

parseVector :: Parser LispVal
parseVector = do
  void (string "#(")
  vec <- sepBy parseExpr sc
  void (char ')')
  pure $ Vector (V.fromList vec)

charsEscapeMap :: [(Char, Char)]
charsEscapeMap = [('\\', '\\'), ('"', '"')]

charsWhiteSpaceMap :: [(Char, Char)]
charsWhiteSpaceMap = [('n', '\n'), ('r', '\r'), ('t', '\t')]

charMapF :: (Char, Char) -> Parser Char
charMapF (c, r) = char c $> r

escaped :: Parser Char
escaped = do
  void (char '\\')
  choice . map charMapF $ charsEscapeMap ++ charsWhiteSpaceMap

symbol :: Parser Char
symbol = oneOf ("!$%&|*+-/:<=>?@^_~" :: [Char])

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockCommentNested "#|" "|#")
