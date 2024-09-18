module JoinList where

import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
tag :: (Monoid m) => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = Append (tag j1 <> tag j2) j1 j2

-- Exercise 2
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex i _ | i < 0 = Nothing
safeIndex 0 (x : _) = Just x
safeIndex i (_ : xs) = safeIndex (i - 1) xs

(!!?) :: [a] -> Int -> Maybe a
(!!?) i x = x `safeIndex` i

getSizeJl :: (Monoid m, Sized m) => JoinList m a -> Int
getSizeJl = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i j | i < 0 || i >= getSizeJl j = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ j1 j2)
  | i < sizej1 = indexJ i j1
  | otherwise = indexJ (i - sizej1) j2
  where
    sizej1 = getSizeJl j1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ _ s@(Single _ _) = s
dropJ i j@(Append _ j1 j2)
  | i >= getSizeJl j = Empty
  | i >= sizej1 = dropJ (i - sizej1) j2
  | i > 0 = dropJ i j1 +++ j2
  | otherwise = j
  where
    sizej1 = getSizeJl j1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0 = Empty
takeJ _ s@(Single _ _) = s
takeJ i j@(Append _ j1 j2)
  | i >= getSizeJl j = j
  | i >= sizeJl = j1 +++ takeJ (i - sizeJl) j2
  | otherwise = takeJ i j1
  where
    sizeJl = getSizeJl j1

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldr (\s acc -> jlMaker s +++ acc) Empty . lines
    where
      jlMaker :: String -> JoinList (Score, Size) String
      jlMaker s = Single (scoreString s, Size 1) s
  line = indexJ
  replaceLine i s b = takeJ i b +++ fromString s +++ dropJ (i + 1) b
  numLines = getSizeJl
  value = getScore . fst . tag

main = runEditor editor (fromString "a\nb\nc\nd\ne\nf" :: (JoinList (Score, Size) String))
