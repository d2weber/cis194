{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List (tails, transpose)

skips :: [a] -> [[a]]
skips l =
  [ map head $
      takeWhile (not . null) $
        iterate (drop n) $
          drop (n - 1) l
    | n <- [1 .. length l]
  ]

localMaxima :: [Integer] -> [Integer]
localMaxima l = [b | (a, b, c) <- wins, a < b, b > c]
  where
    wins = zip3 l (drop 1 l) (drop 2 l)

-- alternative wit generalized windowing function
window :: Int -> [a] -> [[a]]
window n = foldr (zipWith (:)) (repeat []) . take n . tails

localMaxima2 :: [Integer] -> [Integer]
localMaxima2 l = [b | [a, b, c] <- window 3 l, a < b, b > c]

histogram :: [Integer] -> String
histogram lst =
  unlines $
    reverse
      ( [['0' .. '9'], replicate 10 '=']
          ++ [ [if i `elem` line then '*' else '.' | i <- [0 .. 9]]
               | line <- transpose $ [filter (== i) lst | i <- [0 .. 9]]
             ]
      )
