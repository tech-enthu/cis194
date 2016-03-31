module Main where

import StringBuffer
import Editor
import Sized
import Scrabble
import Buffer
import JoinList

{-
main = runEditor editor $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
-}
main = runEditor editor $ (fromString "test" :: (JoinList (Score,Size) String)) 
