module Golf where
import Data.List

--ex1
--With recursion
--Call another sub function called skips' which takes 2 parameters namely list and the length of it and reverse the resultant output 
skips :: [a] -> [[a]]
skips xs = reverse $ skips' xs $ length xs

--skips' will return a list of lists
skips' :: [a] -> Int -> [[a]]
skips' xs 1 = [xs]
skips' [] _ = [[]]
skips' xs n = (nEle n xs) : skips' xs (n-1) 


--return every nth element
nEle :: Int -> [a] -> [a]
nEle n xs = [x | (x,i) <- zip xs [1,2..], mod i n == 0]

--Without recursion
--This is the same as skips but written in more elegant way
-- 1) replicate the list number of times equal to its length and assign index to each individual list
-- 2) for every element from step1 map function nEle2
skips2 :: [a] -> [[a]]
skips2 xs = map nEle2 $ zip (replicate (length xs) xs) [1,2..]

--Takes a list & integer (say n) tuple and returns a list matching every nth element
nEle2 :: ([a],Int) -> [a]
nEle2 (xs,n) = [x | (x,i) <- zip xs [1,2..], mod i n == 0] 

--ex2
--Local maxima list
localMaxima :: [Integer] -> [Integer]
localMaxima xs = concat $ map (\(x:y:z:[]) -> if y > x && y > z then [y] else []) $ filter (\x -> (length x >= 3)) $ map (take 3) $ lMax' (length xs) xs

lMax' :: Int -> [Integer] -> [[Integer]]
lMax' 0 _ = []
lMax' _ [] = []
lMax' n (x:xs) = (x:xs) : lMax' (n-1) xs

--v2 of localMaxima2 
localMaxima2 :: [Integer] -> [Integer]
localMaxima2 xs = concat $ map (\(x:y:z:[]) -> if y > x && y > z then [y] else []) $ filter (\x -> (length x >= 3)) $ map (take 3) $ scanr1 (++) $ map (:[]) xs


--ex3
histogram :: [Integer] -> String
{-a)li processing: 1)append digits 0-9 to the input so that all digits are present atleast once
                   2)remove any number outside the range of 0-9
                   3)sort the digits so that same digits can be grouped together into a sublist
                   4)group the same digits into sublist.
                   5)if input is [1,1,1,5], li will contain [[0],[1,1,1],[2],[3],[4],[5,5],[6],[7],[8],[9]]
  b)li_len : 1) replace each sublist with the length of it
             2) li_len will contain [1,3,1,1,1,2,1,1,1,1] 1st element will always contain length of digit 0, 2nd element of digit 1 and so on ..
  c)max_len: 1) it will store the maximum value from the list li_len.
             2) above example, it will contain 3
  d)di_star processing: 1) For each element in list li_len
                            1a) create a sublist which will contain 
                                 1a1) ' ' (space) replicated by (max_len - (length -1)) times. This is needed for formatting purposes and display alignment
                                 1a2) '*' replicated by (length-1) times (-1 because to offset the extra digit added in step1)
                        2) Finally each sublist (one sublist for each digit)  will have the same length ie number of '*' + number of ' ' == max_len
                        3) di_star will contain ["   ","***","   ","   ","   ","  *","   ","   ","   ","   "]
  e)tr_di processing: 1) transpose the list of lists (ie di_star)
                      2) tr_di will contain [" *        "," *        "," *   *     "]
  f)ba: ba will contain "==========\n0123456789\n" (Its hard coded value)
  h) unlines tr_di : 1) essentially it appends all the sublists in tr_di with "\n" as seperator
                     2) It will contain " *        \n *        \n *   *     \n"
  i)final output: 1) append f) to h)
                  2) final output will be " *        \n *        \n *   *     \n==========\n0123456789\n"i -}

histogram xs = let li = (group . sort . filter ((\x -> if x >=0 && x <=9 then True else False))) $ xs ++ [0,1..9]
                   li_len = map length li
                   max_len = maximum li_len
                   di_star = map (\x -> (replicate (max_len-x+1) ' ') ++ (replicate (x-1) '*') ) li_len
                   tr_di = transpose di_star
                   ba = (replicate 10 '=') ++ "\n0123456789\n"
               in unlines tr_di ++ ba

