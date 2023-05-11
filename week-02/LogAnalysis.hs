{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage m = case (words m) of
  "I" : time : msg -> LogMessage Info (read time) (unwords msg)
  "W" : time : msg -> LogMessage Warning (read time) (unwords msg)
  "E" : sever : time : msg -> LogMessage (Error (read sever)) (read time) (unwords msg)
  msg -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

timeStamp :: LogMessage -> Int
timeStamp (Unknown _) = -1
timeStamp (LogMessage _ time _) = time

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left other right)
  | timeStamp msg < timeStamp other = Node (insert msg left) other right
  | otherwise = Node left other (insert msg right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

isInterestingErr :: LogMessage -> Bool
isInterestingErr (LogMessage (Error s) _ _) = s > 50
isInterestingErr _ = False

msgText :: LogMessage -> String
msgText (Unknown m) = m
msgText (LogMessage _ _ m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map msgText (inOrder . build $ filter isInterestingErr ms)
