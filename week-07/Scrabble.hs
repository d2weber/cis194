{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char

newtype Score = Score {getScore :: Int} deriving (Eq, Show)

scoreString :: String -> Score
scoreString = mconcat . map score

instance Semigroup Score where
  (Score x) <> (Score y) = Score (x + y)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c = Score $ case toLower c of
  'a' -> 1
  'b' -> 3
  'c' -> 3
  'd' -> 2
  'e' -> 1
  'f' -> 4
  'g' -> 2
  'h' -> 4
  'i' -> 1
  'j' -> 8
  'k' -> 5
  'l' -> 1
  'm' -> 3
  'n' -> 1
  'o' -> 1
  'p' -> 3
  'q' -> 10
  'r' -> 1
  's' -> 1
  't' -> 1
  'u' -> 1
  'v' -> 4
  'w' -> 4
  'x' -> 8
  'y' -> 4
  'z' -> 10
  _ -> 0
