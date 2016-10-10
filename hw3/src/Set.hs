{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Set where
import qualified Data.Foldable as F
import qualified Tree as T
import Data.Monoid ((<>))

class (Eq a, Ord a) => Set s a where
    emptySet :: s a
    toList   :: s a -> [a]
    find     :: a -> s a -> Maybe a
    insert   :: a -> s a -> s a
    delete   :: a -> s a -> s a
    next     :: a -> s a -> Maybe a
    fromList :: [a] -> s a


instance (Eq a, Ord a) => Set T.Tree a where
    emptySet   = mempty
    toList     = foldr (:) []
    find   x   = F.find (== x)
    insert x   = (<>) $ fromList [x]
    delete x t = T.delete t x
    next   x   = F.find (> x)
    fromList   = T.fromList
