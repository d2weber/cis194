{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Applicative
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

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

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}
  deriving (Show)

-- Exercise 2
battle :: Battlefield -> Rand StdGen Battlefield
battle Battlefield {attackers = as, defenders = ds} = do
  a <- dies_a
  d <- dies_d
  return $ Battlefield (as - count (<=) a d) (ds - count (>) a d)
  where
    dies_a = n_sorted $ min 3 $ max 0 $ as - 1
    dies_d = n_sorted $ min 2 ds
    n_sorted n = reverse . sort <$> replicateM n die
    count p a d = length $ filter id $ zipWith p a d

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade b@Battlefield {attackers, defenders}
  | attackers < 2 || defenders < 1 = return b
invade b = battle b >>= invade

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb b = calcRatio <$> replicateM 1000 (invade b)
  where
    calcRatio = (/ 1000) . fromIntegral . length . filter success
    success Battlefield {defenders = 0} = True
    success _ = False
