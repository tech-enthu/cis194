{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
import qualified Data.List as L
--ex1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0,1..]

--ex2
fibs2 :: [Integer]
fibs2 = 0:1:fibs2' 0 1

fibs2' n1 n2 = let n3 = n1+n2
               in n3 : fibs2' n2 n3

--ex3
data Stream a = Cons a (Stream a)

listToStream :: [a] -> Stream a
listToStream (x:xs) = Cons x (listToStream xs)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a:streamToList b

instance Show a => Show (Stream a) where
  show x = (show . take 120 . streamToList) x  

--ex4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x) 

--ex5
nats :: Stream Integer
nats = streamFromSeed (1+) 0

rulerEle :: Integer -> Stream Integer -> Integer
rulerEle x = round . logBase 2 . fromIntegral . maximum . filter (\y-> mod x y == 0) . takeWhile (<=x) . streamToList 

ruler :: Stream Integer
ruler = let multStream = streamFromSeed (2*) 1  --Stream of 2^x (ie 1,2,4,8,16,32..)
            intStream = streamFromSeed (+1) 1   --Stream of integers starting from 1 (ie 1,2,3,4..)
        in streamMap (flip rulerEle multStream) intStream

--ex6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger x = Cons x (streamRepeat 0)
  negate (Cons x xs) = Cons (-x) (negate xs)
  (+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs+ys)
  (*) (Cons x xs) (Cons y ys) = Cons (x*y) ((streamMap (x*) ys) + xs*ys)
--  (*) (Cons a as) (Cons b bs) = (a * b) + x * (a * bs + b * as + as * bs) <mathematical representation>

--instance Fractional (Stream Integer) where 
--  (/) (Cons a as) (Cons b bs) = (a/b) + (1/b) * (as - 


--ex7

--matrix multiplication

--takes 2 lists of lists (ie matrices) and returns the multiplication of them 
matMult :: [[Integer]] -> [[Integer]] -> [[Integer]]
matMult rs cs = let cs' = L.transpose cs
                    m' =  map (matMult' cs') rs
                    zipRC = map (map $ \(x,y) -> zip x y) m'
                    mult = map (map $ map $ \(x,y) -> x * y) zipRC
                in map (map sum) mult

--takes a list of lists & a list and returns a list of pairs of Integer lists 
matMult' :: [[Integer]] -> [Integer] -> [([Integer],[Integer])]
matMult' cs r = map (\c -> (r,c)) cs    

--Fibonacci via matrix
data Matrix a = Matrix [[a]]

instance Num (Matrix Integer) where
  (*) (Matrix m1) (Matrix m2) = Matrix $ matMult m1 m2 

baseMatrix :: Matrix Integer
baseMatrix = Matrix [[1,1],[1,0]]

fib4 :: Integer -> Integer
fib4 0 = (\(Matrix [_,[_,f]]) -> f) $ baseMatrix      --it can be hardcoded to 0.. but just for completeness, I extracted it from matrix :)
fib4 n = (\(Matrix [[_,f],_]) -> f) $ baseMatrix ^ n 
