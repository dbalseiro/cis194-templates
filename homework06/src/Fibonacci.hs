----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 06
--
----------------------------------------------------------------------

module Fibonacci where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

fibs2 :: [Integer]
fibs2 = map snd $ iterate (\(a,b) -> (b, a+b)) (0,1)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream head tail) = head:(streamToList tail)


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream head tail) = Stream (f head) (streamMap f tail)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Stream (f seed) (streamFromSeed f (f seed))


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

odds :: Stream Integer
odds = streamFromSeed (+2) (-1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream head1 tail1) stream2 = 
    Stream head1 (interleaveStreams stream2 tail1)

ruler :: Stream Integer
ruler = foldr interleaveStreams (streamRepeat 0) (map streamRepeat [1..])

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

x :: Stream Integer
x = undefined


----------------------------------------------------------------------
-- Exercise 7 (Optional)
----------------------------------------------------------------------

fib4 :: Integer -> Integer
fib4 = undefined
