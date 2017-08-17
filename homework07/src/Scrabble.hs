----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 07
--
----------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char
import Data.List

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

scrabbleTable :: [(Char, Integer)]
scrabbleTable = zip ['A'..'Z'] [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

findScore :: Char -> Score
findScore c = case find (\(x,_) -> x == c) scrabbleTable of
    Nothing -> Score 0
    (Just (_,n)) -> Score (fromIntegral n)

score :: Char -> Score
score = findScore . toUpper

scoreString :: String -> Score
scoreString = foldr (\x acc -> mappend (score x) acc) (Score 0)

