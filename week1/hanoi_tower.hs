type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi i a b c = hanoi (i-1) a c b ++ [(a,b)] ++ (hanoi (i-1) c b a)
