{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (liftM2, (>>=))
import Data.List (sort, unfoldr)
import Data.Functor ((<$>))

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercise 2
randList :: Int -> Rand StdGen [Int]
randList n  | n == 0    = return []
            | otherwise = getRandom >>= (\i -> (i:) <$> randList (n-1))

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield attN defN) = foldr ($) bf <$> losses
  where
    losses = liftM2 (zipWith fight) (sort <$> randList attN) (sort <$> randList defN)
    fight att def | def >= att = \(Battlefield a d) -> Battlefield (a-1) d
                  | otherwise  = \(Battlefield a d) -> Battlefield a    (d-1)

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield attN defN)
  | attN < 2 || defN <= 0 = return bf
  | otherwise             = battle bf >>= invade

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = undefined
  where
    invasions = map invade $ replicate 1000 bf
    success (Battlefield att def) | def == 0  = 1
                                  | otherwise = 0