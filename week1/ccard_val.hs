--ex1
toDigits :: Integer -> [Integer]
toDigits x
   | x <= 0 = [] 
   | x < 10 = [x]
   | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
   | x <= 0 = []
   | x < 10 = [x]
   | otherwise = x `mod` 10 : (toDigitsRev $ x `div` 10)

--ex2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = doubleEveryOther' xs $ length xs

doubleEveryOther' _ 0 = []
doubleEveryOther' [] _ = []
doubleEveryOther' (x:xs) l 
    | l `mod` 2 == 0 = (x*2):(doubleEveryOther' xs $ l-1)
    | otherwise = x:(doubleEveryOther' xs $ l-1)


--ex3
sumDigits :: [Integer] -> Integer
sumDigits xs = let a = sumDigits' xs
               in sumDigits'' a 

sumDigits' [] = []
sumDigits' (x:xs) = toDigits x ++ sumDigits' xs

sumDigits'' [] = 0
sumDigits'' (x:xs) = x + sumDigits'' xs


--ex4
validate :: Integer -> Bool
validate x = let a = (sumDigits . doubleEveryOther . toDigits) x
             in a `mod` 10 == 0 


--ex5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi i a b c = hanoi (i-1) a c b ++ [(a,b)] ++ (hanoi (i-1) c b a)
