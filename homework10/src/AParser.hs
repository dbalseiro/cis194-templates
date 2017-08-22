{-# LANGUAGE InstanceSigs #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 10
--
----------------------------------------------------------------------

module AParser where

-- base
import Control.Applicative
import Data.Char

import Control.Arrow

newtype Parser a =
  Parser { runParser :: String -> Maybe (a, String) }

data Example = Example Integer Char
  deriving Show

-- |
--
-- >>> runParser (satisfy isUpper) "ABC"
-- Just ('A',"BC")
-- >>> runParser (satisfy isUpper) "abc"
-- Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f []          = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing


-- |
--
-- >>> runParser (char 'x') "xyz"
-- Just ('x',"yz")

char :: Char -> Parser Char
char c = satisfy (== c)


posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------
    -- newtype Parser a =
    --   Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap transformer (Parser pFunc) = Parser (fmap (first transformer) . pFunc)
    -- = Parser (\str -> first transformer <$> parserFunc str)

    -- where
    --    newParserFunc str = first transformer <$> parserFunc str
            --case parserFunc str of
            --  Nothing -> Nothing
            --  Just (parsedResult, restString) -> Just (transformer parsedResult, restString)

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\str -> Just (a, str))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser func1) <*> (Parser func2) = Parser (mapping . func2)
    where
      mapping Nothing = Nothing
      mapping (Just (a, rest)) = first ($ a) <$> func1 rest

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> runParser abParser "abcdef"
-- Just (('a','b'),"cdef")
-- >>> runParser abParser "aebcdf"
-- Nothing

abParser :: Parser (Char, Char)
abParser = join <$> char 'b' <*> char 'a'
    where join b a = (a, b)

{--
    where f str = case str of
                   ('a':'b':rest) -> Just (('a', 'b'), rest)
                   _ -> Nothing
--}


-- |
--
-- >>> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- >>> runParser abParser_ "aebcdf"
-- Nothing

abParser_ :: Parser ()
abParser_ = join <$> char 'b' <*> char 'a'
    where join _ _ = ()
{--
    where f str = case str of
                   ('a':'b':rest) -> Just ((), rest)
                   _ -> Nothing
--}

--main :: IO()
--main = putStr $ show $ runParser abParser_ "abctopu"


-- |
--
-- >>> runParser intPair "12 34"
-- Just ([12,34],"")

intPair :: Parser [Integer]
intPair = join <$> intEspacio <*> intEspacio
    where
        intEspacio = (\_ i -> [i]) <$> char ' ' <*> posInt
        join = flip (++)

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser f1) <|> (Parser f2) = Parser fresult
      where fresult str = f1 str <|> f2 str

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- 
--
-- >>> runParser intOrUppercase "342abcd"
-- Just ((),"abcd")
-- >>> runParser intOrUppercase "XYZ"
-- Just ((),"YZ")
-- >>> runParser intOrUppercase "foo"
-- Nothing

intOrUppercase :: Parser ()
intOrUppercase = intPosEmpty <|> isUpperEmpty
    where
        isUpperEmpty = const () <$> satisfy isUpper
        intPosEmpty = const () <$> posInt
