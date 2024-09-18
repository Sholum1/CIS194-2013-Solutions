{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Foldable (fold)
import Data.Tree
import Employee
import Text.Printf

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fs) = GL (e : es) (fs + empFun e)

instance Semigroup GuestList where
  (<>) (GL es1 fs1) (GL es2 fs2) = GL (es1 ++ es2) (fs1 + fs2)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend = (<>)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f e t = f (rootLabel t) $ map (treeFold f e) (subForest t)

-- Exercise 3
nextLevel ::
  Employee ->
  [(GuestList, GuestList)] ->
  (GuestList, GuestList)
nextLevel b gLs =
  (glCons b $ foldMap snd gLs, foldMap (uncurry moreFun) gLs)

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel (mempty, mempty)

-- Exercise 5
main :: IO ()
main = readFile "company.txt" >>= putStr . glToString . maxFun . read

glToString :: GuestList -> String
glToString (GL es f) =
  printf
    "Total Fun: %d\n%s"
    f
    (unlines $ map empName es)

-- Extra
combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs b gLs =
  let bL = GL [b] (empFun b)
   in moreFun bL $ fold gLs

-- List of (Boss, Employees)
treeToBEs :: Tree Employee -> [(GuestList, GuestList)]
treeToBEs t = treeToBE t : concatMap treeToBEs (subForest t)
  where
    treeToBE :: Tree Employee -> (GuestList, GuestList)
    treeToBE t' =
      ( glCons (rootLabel t') mempty,
        foldr
          (glCons . rootLabel)
          mempty
          (subForest t')
      )

-- treeFold like foldr
treeFold' :: (a -> b -> b) -> b -> Tree a -> b
treeFold' f e t =
  foldr (flip $ treeFold' f) (rootLabel t `f` e) (subForest t)
