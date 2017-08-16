{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import Log
import Text.Read

data ValidMessageType = ValidMessageType MessageType TimeStamp [String]
                      | InvalidMessageType
                      deriving Eq

-- Try to get the message type.. if it is not valid, then the log is unknown
parseMessage :: String -> LogMessage
parseMessage message = 
    case parseMessageType (words message) of
      InvalidMessageType -> Unknown message
      ValidMessageType messageType timestamp rest -> 
        LogMessage messageType timestamp (unwords rest)

-- for each message type, i try to get the timestamp
-- for error messages, try to get the rror code first
parseMessageType :: [String] -> ValidMessageType
parseMessageType [] = InvalidMessageType
parseMessageType ("I":rest) = getTimeStamp Info rest
parseMessageType ("W":rest) = getTimeStamp Warning rest
parseMessageType ("E":rest) = getError rest
parseMessageType _ = InvalidMessageType

-- try to get first the error code and then the timestamo
-- if the error code is not valid returns Invalid
getError :: [String] -> ValidMessageType
getError [] = InvalidMessageType
getError (errorCode:rest) = 
  case validInt errorCode of
    InvalidInt -> InvalidMessageType
    (ValidInt i) -> getTimeStamp (Error i) rest

--try to get timestamp if valid
getTimeStamp :: MessageType -> [String] -> ValidMessageType
getTimeStamp _ [] = InvalidMessageType
getTimeStamp messageType (timestamp:rest) =
  case readMaybe timestamp of
    Nothing -> InvalidMessageType
    Just validInt -> ValidMessageType messageType validInt rest

parse :: String -> [LogMessage]
parse = parse' . lines


parse' :: [String] -> [LogMessage]
parse' [] = []
parse' (x:xs) = parseMessage x : parse' xs

-- inserts a new LogMessage into an existing MessageTree, pro- ducing a 
-- new MessageTree. insert may assume that it is given a sorted MessageTree, 
-- and must produce a new sorted MessageTree containing the new LogMessage in 
-- addition to the contents of the original MessageTree.
-- However, note that if insert is given a LogMessage which is Unknown, it 
-- should return the MessageTree unchanged.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert (LogMessage _ _ _) tree@(Node _ (Unknown _) _) = tree
insert logMessage@(LogMessage _ timestamp _) (Node left content@(LogMessage _ treeTimestamp _) right) =
    if timestamp < treeTimestamp
        then Node (insert logMessage left) content right 
        else Node left content (insert logMessage right)

--which builds up a MessageTree containing the messages in the list, by 
--successively inserting the messages into a MessageTree (beginning with a Leaf).
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- takes a sorted MessageTree and produces a list of all the LogMessage
-- s it contains, sorted by timestamp from smallest to biggest.- (This is known as an
-- in-order traversal of the MessageTree .)
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left content right) = (inOrder left) ++ [content] ++ (inOrder right)

--  takes an unsorted list of LogMessage s, and returns a list of the
--  messages corresponding to any errors with a severity of 50 or greater,
--  sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error errorLevel) _ message):rest)
  | errorLevel >= 50 = message : whatWentWrong rest
  | otherwise = whatWentWrong rest
whatWentWrong (_:rest) = whatWentWrong rest


