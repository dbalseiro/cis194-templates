{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Arrow(first)
import Control.Monad.Random
import Control.Monad

import Data.List
import Data.Bool

--
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Num, Ord, Show)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom


----------------------------------------------------------------------
-- Risk
----------------------------------------------------------------------

type Army =
  Int


data Battlefield =
  Battlefield
    { attackers :: Army
    , defenders :: Army
    }
    deriving Show


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

diceValues :: Int -> Rand StdGen [DieValue]
diceValues n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle battleField =
    let calcDiceDefenders :: Rand StdGen [DieValue]
        calcDiceDefenders = diceValues $ min 3 (defenders battleField)

        calcDiceAttackers :: Rand StdGen [DieValue]
        calcDiceAttackers = diceValues $ min 3 (bool qty (qty - 1) (qty <= 3))
            where qty = attackers battleField

        sortedDice :: Rand StdGen [DieValue] -> Rand StdGen [DieValue]
        sortedDice = fmap (sortBy (flip compare))
     in do
         attackersDice <- sortedDice calcDiceAttackers
         defendersDice <- sortedDice calcDiceDefenders
         return $ skirmish battleField attackersDice defendersDice
    where
        skirmish :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
        skirmish bf att def = skirm bf $ map (bool (-1) 0) $ zipWith (>) att def

        skirm :: Battlefield -> [Int] -> Battlefield
        skirm = foldr deaths

        -- x == -1 => Defenders Won
        deaths :: Int -> Battlefield -> Battlefield
        deaths x bf = Battlefield (attackers bf + x) (defenders bf - (x + 1))

main :: IO ()
main = do
    putStr "Attacker Size: "
    attackerSize <- readLn :: IO Int
    putStr "Defender Size: "
    defenderSize <- readLn :: IO Int
    let initialBF = Battlefield attackerSize defenderSize
    prob <- successProb initialBF
    print prob

consume :: Battlefield -> IO ()
consume _ = putStr ""

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

invade :: (Battlefield -> IO ()) -> Battlefield -> IO Battlefield
invade action bf
    | attackers bf <= 1 || defenders bf == 0 = return bf
    | otherwise = do
        b <- evalRandIO $ battle bf
        action b
        invade action b

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

successProb :: Battlefield -> IO Double
successProb bf = do
    results <- replicateM 1000 $ invade consume bf
    let successes = length $ filter ((== 0) . defenders) results
        prob = (fromIntegral successes / 1000) * 100
    return prob