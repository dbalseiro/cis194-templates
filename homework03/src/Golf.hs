----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 03
--
----------------------------------------------------------------------

module Golf where

import Data.List

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
-- >>> skips [1]
-- [[1]]
-- >>> skips [True, False]
-- [[True,False],[False]]
-- >>> skips []
-- []

skips :: [a] -> [[a]]
skips l = map skipMap (zip [1..(length l)] (repeat l))

skipMap :: (Int, [a]) -> [a]
skipMap l = case fst l of
    0 -> (snd l)
    n -> foldr (\x acum -> 
        if (fst x) `mod` n == 0
            then (snd x):acum
            else acum) [] (zip [1..(length (snd l))] (snd l))
----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima [2,3,4,1,5]
-- [4]
-- >>> localMaxima [1,2,3,4,5]
-- []
localMaxima :: [Integer] -> [Integer]
localMaxima list
    | length list <= 2 = []
    | (maximum (take 3 list)) == (take 3 list) !! 1 = (take 3 list) !! 1 : localMaxima (drop 2 list)
    | otherwise = localMaxima (drop 1 list)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> histogram [1,1,1,5]
-- " *        \n *        \n *   *    \n==========\n0123456789\n"

data Occurences = Occ [Integer] Occurences | Empty
  deriving Show

createHash :: Integer -> Occurences -> Occurences
createHash i Empty = Occ [i] Empty
createHash i (Occ values occ)
  | i `elem` values = Occ values (createHash i occ)
  | otherwise = Occ (i:values) occ

asterisks :: Occurences -> [String]
asterisks Empty = []
asterisks (Occ values occ) = draw (sort values) 0 : asterisks occ

draw :: [Integer] -> Int -> String
draw _ 10 = ""
draw [] _ = ""
draw (x:xs) pos
  | (fromIntegral x) == pos = "*" ++ (draw xs (pos + 1))
  | otherwise = " " ++ (draw (x:xs) (pos + 1))

histogram :: [Integer] -> String
histogram = unlines . reverse . drawText

drawText :: [Integer] -> [String]
drawText list = ["0123456789", "=========="] ++ (asterisks $ createOccurences list)

createOccurences :: [Integer] -> Occurences
createOccurences list = foldr createHash Empty list
  
