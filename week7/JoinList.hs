{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module JoinList where
import qualified Data.Monoid as M
import Sized
import Scrabble
import Buffer


data JoinList m a = Empty
                     | Single m a
                     | Append m (JoinList m a) (JoinList m a)
               deriving (Eq, Show)

tag :: M.Monoid m => JoinList m a -> m
tag Empty = M.mempty
tag (Single m _) = m
tag (Append m _ _) = m

jlFold :: b -> (m -> a -> b) -> (m -> b -> b -> b) -> JoinList m a -> b
jlFold e _ _ Empty = e
jlFold _ f _ (Single m a) = f m a 
jlFold e f g (Append m l r) = g m (jlFold e f g l) (jlFold e f g r) 

--ex1
(+++) :: M.Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (M.mappend (tag a) (tag b)) a b

--ex2
--   1
indexJ :: (Sized b, M.Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i <= 0 = Nothing
indexJ 1 (Single _ a) = Just a
indexJ i (Append m jlL jlR) =  let lBranchSize = (getSize . size . tag) jlL
                               in if i <= lBranchSize then indexJ i jlL 
                                                      else indexJ (i-lBranchSize) jlR
--   2
dropJ :: (Sized b, M.Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i _ | i < 0 = Empty
dropJ 0 jl = jl
dropJ i (Append m jlL jlR) = let lBranchSize = (getSize . size . tag) jlL
                             in if i < lBranchSize then Append m (dropJ i jlL) jlR
                                                else dropJ (i-lBranchSize) jlR 

--   3
takeJ :: (Sized b, M.Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i < 0 = Empty
takeJ 0 jl = Empty
takeJ i (Append m jlL jlR) = let lBranchSize = (getSize . size . tag) jlL
                             in if i < lBranchSize then takeJ i jlL
                                                   else Append m jlL (takeJ (i-lBranchSize) jlR)

--ex3
scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

{-
      jlFold :: b -> (m -> a -> b) -> (m -> b -> b -> b) -> JoinList m a -> b
      jlFold e _ _ Empty = e
      jlFold _ f _ (Single m a) = f m a
      jlFold e f g (Append m l r) = g m (jlFold e f g l) (jlFold e f g r)
-}

--ex4
instance Buffer (JoinList (Score,Size) String) where
  toString = jlFold "" (\_ s -> s) (\_ l r -> unlines [l,r])

  fromString s = let li = lines s
                     sinList = map (\x -> Single (scoreString x,Size 1) x) li
                 in foldr (+++) Empty sinList 

  line = indexJ 

  replaceLine i s jL = let pre = takeJ (i-1) jL
                           suf = dropJ i jL
                       in pre +++ fromString s +++ suf
  numLines = getSize.snd.tag 

  value = getScore.fst.tag
