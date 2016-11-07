data CartesianTree a = Nil | Node a a (CartesianTree a) (CartesianTree a)
    deriving Show

split :: Ord a => CartesianTree a -> a -> (CartesianTree a, CartesianTree a)
split Nil _ = (Nil, Nil)
split (Node k v l r) key
    | key > k   = (Node k v l (fst splitChildRight), snd splitChildRight)
    | otherwise = (Node k v (snd splitChildLeft) r, fst splitChildLeft)
    where
        splitChildRight = split r key
        splitChildLeft  = split l key

merge :: Ord a => CartesianTree a -> CartesianTree a -> CartesianTree a
merge Nil t2 = t2
merge t1 Nil = t1
merge t1@(Node k1 v1 l1 r1) t2@(Node k2 v2 l2 r2)
    | v1 > v2   = Node k1 v1 l1 (merge r1 t2)
    | otherwise = Node k2 v2 (merge t1 l2) r2

insert :: Ord a => CartesianTree a -> a -> a -> CartesianTree a
insert node k v = merge (snd splitByKey) (merge (Node k v Nil Nil) (fst splitByKey))
    where splitByKey = split node k

remove :: Ord a => CartesianTree a -> a -> CartesianTree a
remove node@(Node k v l r) key
    | k == key = merge l r
    | k  < key = Node k v (remove l key) r
    | k  > key = Node k v l (remove r key)
remove Nil _ = Nil

fromList :: Ord a => [(a,a)] -> CartesianTree a
fromList [] = Nil
fromList l = merge (Node (fst h) (snd h) Nil Nil) (fromList t)
    where
        h = head l
        t = tail l

