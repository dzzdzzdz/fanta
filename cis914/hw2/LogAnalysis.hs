{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage line =
  let wordList = words line in
    case wordList of
      ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
      ("w":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
      ("E":lvl:ts:msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
      _ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
  | ts1 < ts2 = Node (insert msg1 left) msg2 right
  | otherwise = Node left msg2 (insert msg1 right)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error severity) _ _) = severity >= 50
isRelevant _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ text) = text
getMessage (Unknown text) = text

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
  let sortedMessages = inOrder (build messages)
      relevantMessages = filter isRelevant sortedMessages
  in map getMessage relevantMessages
  
