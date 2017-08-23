----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 11
--
----------------------------------------------------------------------

module SExpr where

import AParser

-- base
import Control.Applicative
import Control.Arrow
import Data.Char


----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
-- >>> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
-- Just ("","abcdeFGh")

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore (Parser f) = Parser f'
    where f' str = case f str of
                     Nothing -> Just ([], str)
                     Just (a, rest) -> first (a :) <$> f' rest

-- |
--
-- >>> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
-- >>> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
-- Nothing

atLeastOne :: Parser [a] -> Parser [a]
atLeastOne (Parser f) = Parser f'
    where f' str = case f str of
                     Nothing -> Nothing
                     Just ([], _) -> Nothing
                     x -> x

oneOrMore :: Parser a -> Parser [a]
oneOrMore = atLeastOne . zeroOrMore

---------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

main :: IO()
main = do
    print $ runParser spaces "abcdeFGh"
    print $ runParser spaces "  ABCdEfgH"



-- |
--
-- >>> runParser ident "foobar baz"
-- Just ("foobar"," baz")
-- >>> runParser ident "foo33fA"
-- Just ("foo33fA","")
-- >>> runParser ident "2bad"
-- Nothing
-- >>> runParser ident ""
-- Nothing

exactlyOne :: Parser a -> Parser [a]
exactlyOne (Parser f) = Parser f'
    where f' str = first (:[]) <$> f str

ident :: Parser String
ident = (++) <$> oneAlpha <*> manyAlphaNum
    where
        oneAlpha :: Parser String
        oneAlpha = exactlyOne $ satisfy isAlpha

        manyAlphaNum :: Parser String
        manyAlphaNum = zeroOrMore $ satisfy isAlphaNum


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

type Ident =
  String


data Atom
  = N Integer
  | I Ident
  deriving Show


data SExpr
  = A Atom
  | Comb [SExpr]
  deriving Show


-- |
--
-- >>> runParser parseSExpr "  5  "
-- Just (A (N 5),"")
-- >>> runParser parseSExpr "foo3"
-- Just (A (I "foo3"),"")
-- >>> runParser parseSExpr "((lambda x x) 3)"
-- Just (Comb [Comb [A (I "lambda"),A (I "x"),A (I "x")],A (N 3)],"")
-- >>> runParser parseSExpr "(lambda x x) 3"
-- Just (Comb [A (I "lambda"),A (I "x"),A (I "x")],"3")
-- >>> runParser parseSExpr "(lambda x x"
-- Nothing

parseSExpr :: Parser SExpr
parseSExpr = consumeSpacesAndChar '(' *> (Comb <$> manySExpr) <* consumeSpacesAndChar ')' <* consumeSpaces
         <|> intSexpr
         <|> indentSexpr

manySExpr :: Parser [SExpr]
manySExpr = (++) <$> ((:[]) <$> parseSExpr) <*> Parser f
    where 
        f :: String -> Maybe ([SExpr], String)
        f str = case runParser parseSExpr str of
                  Nothing -> Just ([], str)
                  _ -> runParser manySExpr str

consumeSpacesAndChar :: Char -> Parser SExpr
consumeSpacesAndChar c = (A . I) <$> ((++) <$> spaces <*> exactlyOne (char c))

consumeSpaces :: Parser SExpr
consumeSpaces = (A . I) <$> spaces

intSexpr :: Parser SExpr
intSexpr = consumeSpaces *> ((A . N) <$> posInt)

indentSexpr :: Parser SExpr
indentSexpr = consumeSpaces *> ((A . I) <$> ident)
