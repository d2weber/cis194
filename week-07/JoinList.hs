{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Buffer
import Editor
import Scrabble
import Sized (Size (..), Sized (..), getSize)

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
single :: Monoid m => a -> JoinList m a
single = Single mempty

-- This would be the solution without (Sized m):
-- (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
-- (+++) a b = Append (tag a <> tag b) a b

-- Let's try to get a reasonably balanced tree when concatenating
-- I don't get any performance gains without this. Also, the StringBufEditor
-- Version is already very fast with the provided `carol.txt`. Maybe there
-- is some novel optimization going on.
append :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
append a b = Append (tag a <> tag b) a b

(+++) :: (Sized m, Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty a = a
(+++) a Empty = a
(+++) a@(Append _ l r) b
  | sizeJ a > sizeJ b && sizeJ l < sizeJ r = append l (r +++ b)
(+++) a b@(Append _ l r)
  | sizeJ a < sizeJ b && sizeJ l < sizeJ r = append (a +++ l) r
(+++) a b = append a b

sizedFromList :: [a] -> JoinList Size a
sizedFromList = foldr ((+++) . Single (Size 1)) Empty

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ v) = Just v
indexJ i (Append _ l r)
  | i < sizeJ l = indexJ i l
  | i < sizeJ l + sizeJ r = indexJ (i - sizeJ l) r
indexJ _ _ = Nothing

sizeJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeJ = getSize . size . tag

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x | n <= 0 = x
dropJ n (Append _ l r)
  | n <= sizeJ l = dropJ n l +++ r
  | otherwise = dropJ (n - sizeJ l) r
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ n (Append _ l r)
  | n <= sizeJ l = takeJ n l
  | otherwise = l +++ takeJ (n - sizeJ l) r
takeJ _ x = x

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

-- Exercise 4
scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine s = Single (scoreString s, Size 1) s

-- Let's flex our monoid muscles while implementing fromString/toString
instance (Sized m, Monoid m) => Semigroup (JoinList m a) where
  (<>) = (+++)

instance (Sized m, Monoid m) => Monoid (JoinList m a) where
  mempty = Empty

instance Foldable (JoinList m) where
  foldMap _ Empty = mempty
  foldMap f (Single _ x) = f x
  foldMap f (Append _ l r) = foldMap f l <> foldMap f r

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . foldr (:) []
  fromString = mconcat . map scoreSizeLine . lines
  line = indexJ
  replaceLine n _ jl | n < 0 || n >= sizeJ jl = jl
  replaceLine n s jl = takeJ n jl +++ scoreSizeLine s +++ dropJ (n + 1) jl
  numLines = sizeJ
  value = getScore . fst . tag

main :: IO ()
main = runEditor editor $ scoreSizeLine "This is a new buffer. Type `?` for help."
