{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sized where

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a, b) where
  size = size . snd

-- Define Semigroup instance for Size
-- Starting from GHC 8.4, Monoid now requires that its type also be an
-- instance of Semigroup.
instance Semigroup Size where
  (<>) = (+)

instance Monoid Size where
  mempty = Size 0
  mappend = (<>)
