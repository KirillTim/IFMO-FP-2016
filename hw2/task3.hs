data Tree a = Nil | Node (Tree a) a (Tree a)
  deriving Show

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node t1 v t2) x
  | v == x = Node t1 v t2
  | v  < x = Node t1 v (insert t2 x)
  | v  > x = Node (insert t1 x) v t2

delete :: (Ord a) => Tree a -> a -> Tree a
delete Nil _ = Nil
delete (Node t1 v t2) x
  | x == v = del (Node t1 v t2)
  | x  < v = Node (delete t1 x) v t2
  | x  > v = Node t1 v (delete t2 x)

del :: (Ord a) => Tree a -> Tree a
del (Node Nil v t2) = t2
del (Node t1 v Nil) = t1
del (Node t1 v t2) = Node t1 v2 t2
  where
    v2 = leftistElement t2

leftistElement :: (Ord a) => Tree a -> a
leftistElement (Node Nil v _) = v
leftistElement (Node t1 _ _) = leftistElement t1

find :: (Ord a) => (Tree a) -> a -> Maybe (Tree a)
find Nil _ = Nothing
find node@(Node t1 v t2) x
  | x == v = Just node
  | x < v = find t1 x
  | x > v = find t2 x


fromList :: (Ord a) => [a] -> Tree a
fromList [] = Nil
fromList (h:t) = from (Node Nil h Nil) t
  where
    from tr [] = tr
    from tr (h:t) = from (insert tr h) t

toList::Tree a->[a]
toList Nil = []
toList (Node t1 v t2) = toList t1 ++ [v] ++ toList t2
