{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Control.Monad
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
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

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

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

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
-- Since the first function is applied just one time
-- I modified it to a lambda expression
-- first f (ps, s) = (f ps, s) == (\(ps, s) -> (f ps, s))
instance Functor Parser where
  fmap f p = Parser $ fmap (\(ps, s) -> (f ps, s)) . runParser p

-- Exercise 2
instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (<*>) p1 p2 =
    Parser
      ( \s -> case runParser p1 s of
          Nothing -> Nothing
          Just (ps, s') -> runParser (ps <$> p2) s'
      )

-- Exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- void x = () <$ x
-- (<$) = fmap . const or (<$) = (<$>) . const
-- it replaces the content "inside" a Functor
abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = (\i1 _ i2 -> [i1, i2]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4
instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser (\str -> runParser p1 str <|> runParser p2 str)

-- Exercise 5
intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
