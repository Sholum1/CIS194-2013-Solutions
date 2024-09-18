{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonnaci where

-- Exercise 1
fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 =
  map
    (\n -> round ((phi ** n - psi ** n) / sq5))
    [0 ..]
  where
    sq5, phi, psi :: Double
    sq5 = sqrt 5
    phi = (1 + sq5) / 2
    psi = 1 - phi

-- Exercise 3
data Stream s = Cons s (Stream s)

streamToList :: Stream a -> [a]
streamToList (Cons c cs) = c : streamToList cs

instance Show a => Show (Stream a) where
  show s = init (show $ streamToList s)

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons c cs) = Cons (f c) (streamMap f cs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f cs = Cons cs (streamFromSeed f (f cs))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons c1 cs1) (Cons c2 cs2) =
  Cons
    c1
    ( Cons
        c2
        (interleaveStream cs1 cs2)
    )

ruler :: Stream Integer
ruler = interleaveStream (streamRepeat 0) (streamMap (+ 1) nats)

-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

grau :: Stream Integer -> Integer
grau (Cons c cs) = if c == 0 then 1 + grau cs else 0

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons c cs) = Cons (negate c) (negate cs)
  (+) (Cons c1 cs1) (Cons c2 cs2) = Cons (c1 + c2) (cs1 + cs2)
  (*) (Cons c1 cs1) s2@(Cons c2 cs2) =
    Cons
      (c1 * c2)
      (streamMap (c1 *) cs2 + cs1 * s2)

instance Fractional (Stream Integer) where
  (/) s1@(Cons c1 cs1) s2@(Cons c2 cs2) =
    if grau s1 >= grau s2
      then q
      else undefined
    where
      q =
        if c2 == 0
          then Cons c1 (cs1 / cs2)
          else Cons (div c1 c2) (streamMap (`div` c2) cs1 - q * cs2)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ (2 :: Integer))

-- Exercise 7
data Matrix a = Matrix a a a a deriving (Show)

instance Num (Matrix Integer) where
  fromInteger n = Matrix n n n n
  negate (Matrix x11 x12 x21 x22) =
    Matrix (negate x11) (negate x12) (negate x21) (negate x22)
  (+) (Matrix x11 x12 x21 x22) (Matrix y11 y12 y21 y22) =
    Matrix
      (x11 + y11)
      (x12 + y12)
      (x21 + y21)
      (x22 + y22)
  (*) (Matrix x11 x12 x21 x22) (Matrix y11 y12 y21 y22) =
    Matrix
      (x11 * y11 + x12 * y21)
      (x11 * y12 + x12 * y22)
      (x21 * y11 + x22 * y21)
      (x21 * y12 + x22 * y22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = takeFst $ Matrix 1 1 1 0 ^ n
  where
    takeFst :: Matrix a -> a
    takeFst (Matrix a _ _ _) = a
