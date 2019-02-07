{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values
newtype DieValue = DV
    { unDV :: Int
    } deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
    random = first DV . randomR (1, 6)
    randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk
type Army = Int

data Battlefield = Battlefield
    { attackers :: Army
    , defenders :: Army
    }

rollN :: Int -> Rand StdGen [DieValue]
rollN 0 = return []
rollN n = do
    roll <- die
    next <- rollN (n - 1)
    return (roll : next)

battleResults :: (DieValue, DieValue) -> (Army, Army) -> (Army, Army)
battleResults (a, d) (aAcc, dAcc)
    | diff > 0 = (aAcc, dAcc - 1)
    | otherwise = (aAcc - 1, dAcc)
  where
    diff = unDV a - unDV d

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) = do
    attRoll <- roll 3 att
    defRoll <- roll 2 def
    let (a, d) = foldr battleResults (0, 0) $ zip attRoll defRoll
    return (Battlefield (att-a) (def-d))
  where
    roll armySize army = (sortBy (flip compare)) <$> (rollN $ min armySize army)
