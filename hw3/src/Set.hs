{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Set where
import qualified Data.Foldable as F
import qualified Tree as T
import Data.Monoid ((<>))

class (Eq a, Ord a, Foldable s, Monoid (s a)) => Set s a where
  emptySet :: s a
  emptySet =  mempty
  toList   :: s a -> [a]
  toList   =  foldr (:) []
  find     :: a -> s a -> Maybe a
  find   x =  F.find (== x)
  insert   :: a -> s a -> s a
  insert x =  (<>) $ fromList [x]
  delete   :: a -> s a -> s a
  delete x =  fromList . filter (/= x) . toList
  next     :: a -> s a -> Maybe a
  next   x =  F.find (> x)
  fromList :: [a] -> s a


instance (Eq a, Ord a) => Set T.Tree a where
  insert x t = T.insert t x
  delete x t = T.delete t x
  fromList   = T.fromList
