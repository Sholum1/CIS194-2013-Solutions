type Peg = String

type Move = (Peg, Peg)

-- Exercise 5
hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 0 _ _ _ = []
hanoi3 1 a b _ = [(a, b)]
hanoi3 n a b c = hanoi3 (n - 1) a c b ++ [(a, b)] ++ hanoi3 (n - 1) c b a

-- Exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b c _ = [(a, c), (a, b), (c, b)]
hanoi4 n a b c d = hanoi4 (n - 2) a c d b ++ hanoi3 2 a b d ++ hanoi4 (n - 2) c b a d
