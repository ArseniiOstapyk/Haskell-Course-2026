module CSP.Parser
  ( ParseError(..)
  , parseProgram
  ) where

import CSP.AST
import Control.Applicative (Alternative(..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

newtype ParseError = ParseError { renderParseError :: String }
  deriving (Eq, Show)

data Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (x, rest) <- runParser p s
    pure (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  pf <*> px = Parser $ \s -> do
    (f, rest) <- runParser pf s
    (x, rest') <- runParser px rest
    pure (f x, rest')

instance Monad Parser where
  p >>= f = Parser $ \s -> do
    (x, rest) <- runParser p s
    runParser (f x) rest

instance Alternative Parser where
  empty = Parser $ const (Left "no parse")
  p <|> q = Parser $ \s ->
    case runParser p s of
      Right ok -> Right ok
      Left _ -> runParser q s


parseProgram :: String -> Either ParseError Program
parseProgram input =
  case runParser program input of
    Left err -> Left (ParseError err)
    Right (p, rest) ->
      case dropSpace rest of
        "" -> Right p
        junk -> Left . ParseError $ "unexpected trailing input near: " ++ take 40 junk

program :: Parser Program
program = do
  skip
  decls <- manyP varDecl
  constraints <- manyP constraintDecl
  _ <- symbol "solve"
  _ <- symbol ";"
  skip
  pure (Program (concat decls) constraints)

varDecl :: Parser [VarDecl]
varDecl = do
  _ <- symbol "var"
  names <- sepBy1 identifier (symbol ",")
  _ <- symbol ":"
  dom <- domain
  _ <- symbol ";"
  pure [VarDecl name dom | name <- names]

constraintDecl :: Parser Constraint
constraintDecl = do
  _ <- symbol "constraint"
  c <- naryConstraint <|> binaryConstraint
  _ <- symbol ";"
  pure c

binaryConstraint :: Parser Constraint
binaryConstraint = do
  lhs <- identifier
  op <- binOp
  rhs <- identifier
  pure (Binary op lhs rhs)

naryConstraint :: Parser Constraint
naryConstraint = do
  _ <- symbol "allDifferent"
  xs <- bracketed identifier
  pure (NAry AllDifferent xs)

domain :: Parser Domain
domain = rangeDomain <|> setDomain

rangeDomain :: Parser Domain
rangeDomain = do
  lo <- integer
  _ <- symbol ".."
  hi <- integer
  pure (IntRange lo hi)

setDomain :: Parser Domain
setDomain = DiscreteSet <$> braced value

value :: Parser Value
value = boolVal <|> intVal <|> stringVal <|> bareStringVal
  where
    boolVal = (symbol "true" >> pure (BoolVal True)) <|> (symbol "false" >> pure (BoolVal False))
    intVal = IntVal <$> integer
    stringVal = StrVal <$> quotedString
    bareStringVal = StrVal <$> identifier

binOp :: Parser BinOp
binOp = choice
  [ symbol "==" >> pure Eq
  , symbol "="  >> pure Eq
  , symbol "/=" >> pure NEq
  , symbol "!=" >> pure NEq
  , symbol "<=" >> pure Le
  , symbol ">=" >> pure Ge
  , symbol "<"  >> pure Lt
  , symbol ">"  >> pure Gt
  ]

identifier :: Parser String
identifier = token $ Parser $ \s ->
  case s of
    c:cs | isAlpha c || c == '_' ->
      let (body, rest) = span (\x -> isAlphaNum x || x == '_' || x == '\'') cs
       in Right (c:body, rest)
    _ -> Left $ "expected identifier near: " ++ take 40 s

integer :: Parser Int
integer = token $ Parser $ \s ->
  let (sign, afterSign) = case s of
        '-':xs -> ("-", xs)
        _ -> ("", s)
      (digits, rest) = span isDigit afterSign
   in if null digits
        then Left $ "expected integer near: " ++ take 40 s
        else Right (read (sign ++ digits), rest)

quotedString :: Parser String
quotedString = token $ Parser $ \s ->
  case s of
    '"':xs -> go [] xs
    _ -> Left $ "expected quoted string near: " ++ take 40 s
  where
    go acc [] = Left "unterminated string literal"
    go acc ('"':rest) = Right (reverse acc, rest)
    go acc ('\\':'"':rest) = go ('"':acc) rest
    go acc ('\\':'\\':rest) = go ('\\':acc) rest
    go acc (c:rest) = go (c:acc) rest

braced :: Parser a -> Parser [a]
braced p = do
  _ <- symbol "{"
  xs <- sepBy1 p (symbol ",") <|> pure []
  _ <- symbol "}"
  pure xs

bracketed :: Parser a -> Parser [a]
bracketed p = do
  _ <- symbol "["
  xs <- sepBy1 p (symbol ",") <|> pure []
  _ <- symbol "]"
  pure xs

symbol :: String -> Parser String
symbol x = token $ Parser $ \s ->
  if x `prefixOf` s
    then Right (x, drop (length x) s)
    else Left $ "expected " ++ show x ++ " near: " ++ take 40 s

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

manyP :: Parser a -> Parser [a]
manyP p = someP p <|> pure []

someP :: Parser a -> Parser [a]
someP p = do
  x <- p
  xs <- manyP p
  pure (x:xs)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  first <- p
  rest <- manyP (sep >> p)
  pure (first:rest)

token :: Parser a -> Parser a
token p = skip >> p >>= \x -> skip >> pure x

skip :: Parser ()
skip = Parser $ \s -> Right ((), dropSpace s)

dropSpace :: String -> String
dropSpace [] = []
dropSpace ('/':'/':xs) = dropSpace (drop 1 (dropWhile (/= '\n') xs))
dropSpace ('#':xs) = dropSpace (drop 1 (dropWhile (/= '\n') xs))
dropSpace (c:cs)
  | isSpace c = dropSpace cs
  | otherwise = c:cs

prefixOf :: String -> String -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (a:as) (b:bs) = a == b && prefixOf as bs
