module Harpe.Parser.Combinators where

import Control.Applicative
import Data.Char (isSpace, isAlphaNum, isUpper)
import Data.List (isPrefixOf)

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Left err -> Left err
    Right (x, s') -> Right (f x, s')

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  Parser pf <*> Parser px = Parser $ \s -> case pf s of
    Left err -> Left err
    Right (f, s') -> case px s' of
      Left err -> Left err
      Right (x, s'') -> Right (f x, s'')

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> case p s of
    Left err -> Left err
    Right (x, s') -> runParser (f x) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Left "Parse error"
  Parser p1 <|> Parser p2 = Parser $ \s -> case p1 s of
    Left _ -> p2 s
    Right x -> Right x

char :: Char -> Parser Char
char c = Parser $ \s -> case s of
  (x:xs) | x == c -> Right (c, xs)
  _ -> Left $ "Expected character " ++ show c

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  (x:xs) | p x -> Right (x, xs)
  _ -> Left "Predicate failed"

anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
  (x:xs) -> Right (x, xs)
  _ -> Left "Unexpected EOF"

string :: String -> Parser String
string [] = pure []
string (c:cs) = char c *> string cs *> pure (c:cs)

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Right ((), [])
  _ -> Left "Expected EOF"

peekChar :: Parser (Maybe Char)
peekChar = Parser $ \s -> case s of
  [] -> Right (Nothing, s)
  (x:_) -> Right (Just x, s)

spaces :: Parser String
spaces = many (satisfy isSpace)

spaces1 :: Parser String
spaces1 = many1 (satisfy isSpace)

hspaces :: Parser String
hspaces = many (satisfy (\c -> c == ' ' || c == '\t'))

hspaces1 :: Parser String
hspaces1 = many1 (satisfy (\c -> c == ' ' || c == '\t'))

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

identifier :: Parser String
identifier = (:) <$> satisfy (\c -> isAlphaNum c || c == '_') <*> many (satisfy (\c -> isAlphaNum c || c == '_' || c == '\''))

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parseLine :: Parser String
parseLine = Parser $ \s ->
  let (line, rest) = break (== '\n') s
      rest' = case rest of
        ('\n':r) -> r
        r        -> r
  in Right (trim line, rest')

parseUntilString :: String -> Parser String
parseUntilString target = Parser $ \s ->
  let go [] acc = (reverse acc, [])
      go s'@(c:cs) acc
        | target `isPrefixOf` s' = (reverse acc, s')
        | otherwise              = go cs (c:acc)
      (chunk, rest) = go s []
  in Right (chunk, rest)

parseUntilStringWithout :: String -> String -> Parser String
parseUntilStringWithout reject target = Parser $ \s ->
  let go [] acc = (reverse acc, [])
      go s'@(c:cs) acc
        | target `isPrefixOf` s' = (reverse acc, s')
        | reject `isPrefixOf` s' = (reverse acc, s')
        | otherwise              = go cs (c:acc)
      (chunk, rest) = go s []
  in Right (chunk, rest)

isEventName :: String -> Bool
isEventName s = "on" `isPrefixOf` s && length s > 2 && isUpper (s !! 2)

breakString :: String -> String -> (String, String)
breakString target s = go s ""
  where
    go [] acc = (reverse acc, "")
    go s'@(c:cs) acc
      | target `isPrefixOf` s' = (reverse acc, s')
      | otherwise              = go cs (c:acc)
