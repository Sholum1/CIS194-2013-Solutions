module Golf where

import Data.List

-- Exercise 1
skips :: [a] -> [[a]]
skips xs =
  map
    -- Associates every element from the xs to a number from indexList,
    -- creating a tuple (x, i)
    (\n -> [x | (x, i) <- zip xs indexList, i `mod` n == 0])
    -- Take just the elements where i is divisible by n
    indexList -- Give n a value from the indexList
  where
    -- Define indexList as a list of `Int` s from 1 to the length of the list.
    indexList :: [Int]
    indexList = [1 .. length xs]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x1 : x2 : x3 : xs)
  -- Handle the case where x2 is a local maximum
  | x2 > x1 && x2 > x3 = x2 : localMaxima (x3 : xs)
  -- If it is not, test again for the rest of the list
  | otherwise = localMaxima (x2 : x3 : xs)
-- If the list has less than three items, returns a empty list
localMaxima _ = []

-- Exercise 3
histogram :: [Integer] -> String
histogram xs =
  ( unlines -- Add the line breaks "\n"
      . reverse -- Mirror it
      . takeWhile (elem '*') -- Take lines just while they have stars
      . transpose -- Rotate 90º
      -- To make the transpose function "rotate" the matrix in  90º,
      -- the rows must have the same length, so we make them infinity
      $ map (++ repeat ' ') stars
  )
    ++ base -- Add the base
  where
    base :: String
    base = "0123456789\n==========\n"
    stars :: [String]
    stars =
      map
        ( flip replicate '*' -- Add the stars
            . (\x -> length x - 1) -- Subtracts the value added previously
        )
        . group -- Group the equals
        . sort -- Put the list in crescent order
        $ (xs ++ [0 .. 9]) -- Make sure every number appear in the list
