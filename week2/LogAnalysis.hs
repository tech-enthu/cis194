{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Control.Applicative


--ex1
parseMessage :: String -> LogMessage
parseMessage s = let (x:xs) = words s
                 in if x == "E" then
                       parseMessageE xs
                    else parseMessageO x xs

parseMessageE :: [String] -> LogMessage
parseMessageE (x1:x2:xs) = let eSev = read x1 :: Int
                               ts = read x2 :: Int
                               msg = unwords xs
                           in LogMessage (Error eSev) ts msg

parseMessageO :: String -> [String] -> LogMessage
parseMessageO mT [] = Unknown mT
parseMessageO mT (x1:xs) = let msg = unwords xs
                          in  case mT of
                                "W" -> LogMessage Warning (read x1 :: Int) msg
                                "I" -> LogMessage Info (read x1 :: Int) msg
                                _   -> Unknown $ mT++" "++x1++" "++ msg 

parse :: String -> [LogMessage]
parse x = map parseMessage $ lines x

getTS :: LogMessage -> TimeStamp
getTS (Unknown _) = 0
getTS (LogMessage _ ts _) = ts


--ex2
insert :: LogMessage -> MessageTree -> MessageTree
insert lM Leaf = Node Leaf lM Leaf 
insert (Unknown _) mT = mT
insert lM (Node lC n_lM rC)  = if (getTS n_lM) > (getTS lM)
                                 then Node (insert lM lC) n_lM rC
                                 else Node lC n_lM (insert lM rC)


--ex3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


--ex4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lC n_lM rC) = inOrder lC ++ [n_lM] ++ inOrder rC

isHighSevErrMsg :: Int -> LogMessage -> Bool
isHighSevErrMsg eS_Threshold (LogMessage (Error eS) _ _) = eS >= eS_Threshold
isHighSevErrMsg _ _ = False

getErrTxt :: LogMessage -> String
getErrTxt (LogMessage _ _ txt) = txt 


--ex5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getErrTxt) . (filter $ isHighSevErrMsg 50) . inOrder . build 

testinOrder :: (String -> [LogMessage]) -> ([LogMessage] -> [LogMessage]) -> FilePath -> IO [LogMessage]
testinOrder par inOrd file = inOrd . par <$> readFile file  

testBuild :: (String -> [LogMessage]) -> ([LogMessage] -> MessageTree) -> FilePath -> IO MessageTree
testBuild par bld file = bld . par <$> readFile file
