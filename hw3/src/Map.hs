{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Map where
import qualified Tree as T
import qualified Set as S


class (Ord k, S.Set m (Entry k v)) => Map m k v where
    emptyMap :: m (Entry k v)
    toList   :: m (Entry k v) -> [(k, v)]
    find     :: k -> m (Entry k v) -> Maybe v
    insert   :: (k, v) -> m (Entry k v) -> m (Entry k v)
    delete   :: k -> m (Entry k v) -> m (Entry k v)
    next     :: k -> m (Entry k v) -> Maybe k
    fromList :: [(k, v)] -> m (Entry k v)

data Entry a b = Entry a b
    deriving (Show)

fst' :: Entry a b -> a
fst' (Entry a _) = a
snd' :: Entry a b -> b
snd' (Entry _ b) = b
entryToPair :: Entry k v -> (k, v)
entryToPair (Entry k v) = (k, v)

entryFromPair :: (a,b) -> Entry a b
entryFromPair (f,s) = Entry f s

instance Eq a => Eq (Entry a b) where
    Entry x _ == Entry y _ = x == y

instance Ord a => Ord (Entry a b) where
  Entry x _ `compare` Entry y _ = x `compare` y

instance Ord k => Map T.Tree k v where
  emptyMap = S.emptySet
  toList   = map entryToPair . S.toList
  find x m = fmap snd $ entryToPair <$> S.find (Entry x undefined) m
  insert x = S.insert (entryFromPair x)
  delete x = S.delete (Entry x undefined)
  next x m = fmap fst $ entryToPair <$> S.next (Entry x undefined) m
  fromList = S.fromList . map entryFromPair
