module BST where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

insert :: (Ord a) => a -> Tree a -> Tree a
contains :: (Ord a) => a -> Tree a -> Bool
fromList :: (Ord a) => [a] -> Tree a

insert a Empty = Node a Empty Empty
insert a (Node b l r) 
    | a < b = Node b (insert a l) r
    | otherwise = Node b l (insert a r)

contains a Empty = False
contains a (Node b l r)
    | a == b    = True
    | a < b     = contains a l
    | otherwise = contains a r

fromList a = foldr insert Empty a
