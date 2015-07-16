{-# OPTIONS_GHC -Wall #-}
module LogAnalsys where

import Log

parseMessage :: String -> LogMessage
parseMessage str = let strLst = words str in
                   case strLst of
                    ("I": x: xs)   -> LogMessage Info (read x) (unwords xs)
                    ("W": x: xs)   -> LogMessage Warning (read x) (unwords xs)
                    ("E": n: x:xs) -> LogMessage (Error (read n)) (read x) (unwords xs)
                    _              -> Unknown "Sorry that's not right"  


parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ stamp1 _) (Node left msg2@(LogMessage _ stamp2 _) right)
  | stamp1 > stamp2 = Node left msg2 (insert msg1 right)
  | otherwise = Node (insert msg1 left) msg2 right

build :: [LogMessage] -> MessageTree
build x = foldr (insert) Leaf x

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inorder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong lgmsg = map (\(LogMessage _ _ m) -> m ) (getWorse lgmsg)
        where getWorse = filter  (\(LogMessage _ s _) -> s > 50)