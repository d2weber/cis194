{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1 = []
  | n < 10 = [n]
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- let's reimplement reverse
rev :: [a] -> [a]
rev = foldl (flip (:)) []

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Exercise 2
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [a] = [a]
doubleEveryOtherRev (a : b : rest) = a : 2 * b : doubleEveryOtherRev rest

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))

-- Exercise 3
myconcat :: [[Integer]] -> [Integer]
myconcat [] = []
myconcat ([] : xss) = myconcat xss
myconcat ((x : xs) : xss) = x : myconcat (xs : xss)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (myconcat (map toDigits xs))

-- Exercise 4
validate :: Integer -> Bool
validate xs = sumDigits (doubleEveryOtherRev (toDigitsRev xs)) `mod` 10 == 0

-- Exercise 5
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ (a, b) : hanoi (n - 1) c b a

-- Exercise 6 (This is not working)
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d
  | even n = hanoi4 (floor (fromIntegral (n - 1) / 2)) a d b c ++ hanoi4 (ceiling (fromIntegral (n - 1) / 2)) a c b d ++ (a, b) : hanoi4 (ceiling (fromIntegral (n - 1) / 2)) c b a d ++ hanoi4 (floor (fromIntegral (n - 1) / 2)) d b a c
  | otherwise = hanoi4 (floor (fromIntegral (n - 1) / 2)) a c b d ++ hanoi4 (ceiling (fromIntegral (n - 1) / 2)) a d b c ++ (a, b) : hanoi4 (ceiling (fromIntegral (n - 1) / 2)) d b a d ++ hanoi4 (floor (fromIntegral (n - 1) / 2)) c b a d
