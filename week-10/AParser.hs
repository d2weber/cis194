module AParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor.Compose

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- Exercise 1
instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

-- Exercise 2
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)

  p1 <*> p2 = Parser $ runParser p1 >=> (\(a, s1) -> runParser (fmap a p2) s1)

-- -- Exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser (Integer, Integer)
intPair = (,) <$> posInt <* char ' ' <*> posInt

-- Exercise 4
instance Alternative Parser where
  empty = Parser $ const Nothing
  -- The `p1 <*> p2` uses applicative style for readers `(-> r)`
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

-- Exercise 5
intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
