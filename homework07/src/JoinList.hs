----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 07
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Sized
import Data.Monoid
import Scrabble

import Editor
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
left +++ right = Append (tag left <> tag right) left right

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------
--safe indexing
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ left right) = jlToList left ++ jlToList right

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl | i < 0 || i >= getSizeInsideJL jl = Nothing
indexJ _ Empty        = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ left right)
    | i < s = indexJ i left
    | otherwise = indexJ (i - s) right
  where
    s = getSizeInsideJL left

getSized :: Sized a => a -> Int
getSized = getSize . size

getSizeInsideJL :: (Sized b, Monoid b) => JoinList b a -> Int
getSizeInsideJL = getSized . tag

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Single _ _) 
    | n > 0 = Empty
    | otherwise = jl
dropJ n jl@(Append m left right)
    | n <= 0 = jl
    | n > getSized m = Empty
    | n >= s = dropJ (n - s) right
    | otherwise = dropJ n left +++ right
  where
    s = getSizeInsideJL left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Single _ _) 
    | n == 0 = Empty
    | otherwise = jl
takeJ n jl@(Append m left right)
    | n == 0 = Empty
    | n >= getSized m = jl
    | n <= s = takeJ n left
    | otherwise = left +++ (takeJ (n - s) right)
  where
    s = getSizeInsideJL right

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------
instance Buffer (JoinList (Score, Size) String) where
    toString = foldr (++) "" . jlToList
    
    fromString s = foldr (+++) Empty (map 
        (\line -> Single (scoreString line, Size 1) line) $ lines s)
    
    line = indexJ

    replaceLine nLine str buffer = 
        (takeJ nLine buffer) +++ (fromString str) +++ (dropJ (nLine + 1) buffer)

    numLines = getSizeInsideJL

    value = getScore . fst . tag

main :: IO()
main = runEditor editor dataJoinList

dataJoinList = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]::Buffer (JoinList (Score, Size) String)
