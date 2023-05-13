{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = fib2 0 1 where fib2 a b = a : fib2 b (a + b)

-- Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream e next) = e : streamToList next

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat v = Stream v (streamRepeat v)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream v vs) = Stream (f v) (streamMap f vs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f z = Stream z (streamFromSeed f (f z))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 1

ruler :: Stream Integer
ruler = streamMap (largestPower 0 0) nats
  where
    largestPower stored i n
      | n < 2 ^ i = stored
      | n `mod` 2 ^ i == 0 = largestPower i (succ i) n
      | otherwise = largestPower stored (succ i) n

-- Exercise 6
x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger :: Integer -> Stream Integer
  fromInteger e = Stream e $ streamRepeat 0

  negate = streamMap negate

  (+) (Stream a as) (Stream b bs) = Stream (a + b) (as + bs)

  (*) (Stream a0 a') b@(Stream b0 b') = Stream (a0 * b0) (streamMap (* a0) b' + a' * b)

instance Fractional (Stream Integer) where
  (/) a@(Stream a0 a') b@(Stream b0 b') = Stream (a0 `div` b0) (a' - a / b * b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- Exercise 7
data Matrix = M Integer Integer Integer Integer deriving (Show)

instance Num Matrix where
  (*) :: Matrix -> Matrix -> Matrix
  (*) (M a11 a12 a21 a22) (M b11 b12 b21 b22) =
    M
      (a11 * b11 + a12 * b21)
      (a11 * b12 + a12 * b22)
      (a21 * b11 + a22 * b21)
      (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 n
  | n == 0 = 0
  | otherwise = let M _ r _ _ = M 1 1 1 0 ^ n in r
