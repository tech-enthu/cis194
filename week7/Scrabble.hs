{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where 
import Data.Map as M
import Data.Monoid

newtype Score = Score Int
   deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
   mempty = Score 0
   mappend = (+)

scraPoints :: Map Char Score
scraPoints = let uCase = ['A'..'Z']
                 lCase = ['a'..'z']
                 sPt = Prelude.map Score [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]
             in M.fromList $ (zip uCase sPt) ++ (zip lCase sPt)

score :: Char -> Score
score x = let a = M.lookup x scraPoints 
          in case a of
               Just s -> s
               Nothing -> mempty

scoreString :: String -> Score
scoreString xs = mconcat $ Prelude.map score xs 
