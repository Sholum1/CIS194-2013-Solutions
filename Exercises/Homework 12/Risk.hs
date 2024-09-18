{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

-- Exercise 1
{- I recommend using ghcup to install ghc and Cabal, and then running
  `cabal install --lib MonadRandom` -}

-- Exercise 2
rolls :: Army -> Rand StdGen [DieValue]
rolls n = replicateM n die

compareRolls :: [(DieValue, DieValue)] -> (Army, Army) -> (Army, Army)
compareRolls [] ad = ad
compareRolls (dV : dVs) (atk, def)
  | dVA > dVD = compareRolls dVs (atk, def - 1)
  | otherwise = compareRolls dVs (atk - 1, def)
  where
    dVA, dVD :: DieValue
    (dVA, dVD) = dV

sort' :: (Ord a) => [a] -> [a]
sort' = sortBy (flip compare)

battle :: Battlefield -> Rand StdGen Battlefield
battle b =
  (rolls . max 0 . min 3 $ atkT - 1) >>= \atkR ->
    (rolls . max 0 . min 2 $ defT) >>= \defR ->
      let newAtk, newDef :: Army
          (newAtk, newDef) =
            compareRolls
              (zip (sort' atkR) (sort' defR))
              (atkT, defT)
       in return $ Battlefield newAtk newDef
  where
    atkT, defT :: Army
    atkT = attackers b
    defT = defenders b

-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade b
  | attackers b <= 1 || defenders b <= 0 = return b
  | otherwise = battle b >>= invade

-- Exercise 4
sucessProb :: Battlefield -> Rand StdGen Double
sucessProb b = replicateM 1000 (invade b) >>= sucessProb'
  where
    sucessProb' :: [Battlefield] -> Rand StdGen Double
    sucessProb' bs = let atkV = filter ((== 0) . defenders) bs in
      return $ fromIntegral (length atkV) / 1000
    
