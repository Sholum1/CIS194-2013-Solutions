-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
  | n < 10 = [n]
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l
  | length l <= 1 = l
  | otherwise =
      doubleEveryOther (take (length l - 2) l)
        ++ [2 * last (init l)]
        ++ [last l]

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs)
  | x < 10 = x + sumDigits xs
  | otherwise = sumDigits (toDigits x) + sumDigits xs

-- Exercise 2
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
