{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage message = case words message of
  ("E" : level : timeStamp : msg) ->
    LogMessage
      (Error (read level))
      (read timeStamp)
      (unwords msg)
  ("I" : timeStamp : msg) ->
    LogMessage
      Info
      (read timeStamp)
      (unwords msg)
  ("W" : timeStamp : msg) ->
    LogMessage
      Warning
      (read timeStamp)
      (unwords msg)
  _ -> Unknown message

parse :: String -> [LogMessage]
parse messageList = map parseMessage (lines messageList)

-- Exercise 2
messageTimeStamp :: LogMessage -> Int
messageTimeStamp (Unknown _) = 0
messageTimeStamp (LogMessage _ timeStamp _) = timeStamp

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert message Leaf = Node Leaf message Leaf
insert message (Node messageTree1 logMessage messageTree2)
  | msgTimeStamp < logMsgTimeStamp =
      Node
        (insert message messageTree1)
        logMessage
        messageTree2
  | msgTimeStamp > logMsgTimeStamp =
      Node
        messageTree1
        logMessage
        (insert message messageTree2)
  | otherwise = Node messageTree1 message messageTree2
  where
    msgTimeStamp, logMsgTimeStamp :: Int
    msgTimeStamp = messageTimeStamp message
    logMsgTimeStamp = messageTimeStamp logMessage

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node messageTree1 logMessage messageTree2) =
  inOrder messageTree1
    ++ [logMessage]
    ++ inOrder messageTree2

-- Exercise 5
errorValue :: LogMessage -> Int
errorValue (LogMessage (Error i) _ _) = i
errorValue _ = 0

actualMessage :: LogMessage -> String
actualMessage (LogMessage _ _ message) = message
actualMessage (Unknown message) = message

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logList =
  let inOrderLogs :: [LogMessage]
      inOrderLogs = inOrder $ build logList
   in [actualMessage x | x <- inOrderLogs, errorValue x >= 50]

-- Exercise 6
allTheMessages :: [LogMessage] -> [String]
allTheMessages logList = map actualMessage $ inOrder $ build logList
