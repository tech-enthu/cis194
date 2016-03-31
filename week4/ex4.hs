import Data.List
--exercise 1 - 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) | even x = (x - 2) * fun1 xs
            | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map ((-)2) . filter even

--exercise 1 - 2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 ( n `div` 2)
       | otherwise = fun2 ( 3 * n + 1)

fun2' :: Integer -> Integer
fun2' =   sum . filter even . takeWhile (>1) . (iterate (\x-> if even x then x `div` 2 else (3 * x + 1))) 

--exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
               deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = foldr (newTree) Leaf xs

getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 (Leaf) x (Leaf)
insertNode x (Node h lT val rT) = if (getHeight lT) <= (getHeight rT) 
                                    then Node h (insertNode x lT) val rT
                                    else Node h lT val (insertNode x rT)

recalHeight :: Tree a -> Tree a
recalHeight Leaf = Leaf
recalHeight (Node _ lT x rT) = let lTH = recalHeight lT
                                   rTH = recalHeight rT
                               in Node (1 + max (getHeight lTH) (getHeight rTH)) lTH x rTH 
newTree :: a -> Tree a -> Tree a
newTree eleme x = (recalHeight . insertNode eleme) x

--Exercise 3 - 1
xor :: [Bool] -> Bool
xor = (foldr (\x y -> not (x && y)) False) . filter (==True) 

--Exercise 3 - 2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

--Exercise 3 - 3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

--Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram x = let xs = [1,2.. x] 
                      fXS = nub . filter (<=x) . (map (\(i,j) -> i+j+2*i*j)) . cartProd xs $ xs
                  in map (\a->a*2+1) . nub . filter (<=x) . map fst . filter (\(i,j) -> i /= j) . cartProd xs $ fXS  

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys] 
