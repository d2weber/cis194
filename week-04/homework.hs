{-# OPTIONS_GHC -Wall #-}

import Data.List

-- Exercise 1
fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even

fun2' :: Integer -> Integer
fun2' =
  sum
    . filter even
    . takeWhile (/= 1)
    . iterate collatz
  where
    collatz n
      | even n = n `div` 2
      | otherwise = 3 * n + 1

-- Exercise 2
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldl ins Leaf
  where
    ins Leaf x = Node 0 Leaf x Leaf
    ins (Node h l y r) x
      | hl < hr = Node h (ins l x) y r
      | hl > hr = Node h l y (ins r x)
      | otherwise =
          let l' = ins l x
           in Node (1 + heightOf l') l' y r
      where
        hl = heightOf l
        hr = heightOf r
        heightOf Leaf = -1
        heightOf (Node h' _ _ _) = h'

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldl (/=) False

{- HLINT ignore map' "Use map" -}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ [1 .. n] \\ toRemove
  where
    toRemove =
      [ k
        | i <- [1 .. n],
          j <- [1 .. n],
          i <= j,
          let k = i + j + 2 * i * j,
          k <= n
      ]
