data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

:{
instance Show a => Show (Tree a) where
    show Empty = show "Emprt"
    show (Node a l r) = foldr (++) "" [show a, " ", show l, " ", show r]
:}

:{
instance Eq a => Eq (Tree a) where
   t1 == t2 = case (t1, t2) of
   	(Empty, Empty) -> True;
   	(Empty, (Node a l r)) -> False;
   	((Node a l r), Empty) -> False;
   	((Node a l r), (Node b ll rr)) -> and [a == b, l == ll, r == rr]
:}

-- class  Functor f  where
--    fmap :: (a -> b) -> f a -> f b
:{
instance Functor Tree where
	fmap f Empty = Empty
	fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)
:}
-- *Tree> fmap (+1) $ Node 1 Empty Empty
-- Node 2 Empty Empty

:{
toList :: Tree a -> [a]
toList Empty = []
toList (Node a l r) = toList l ++ [a] ++ toList r
:}




-- nie da się zdefiniować map
data ToInt a = Fun (a -> Int)


-- rozwiązania


filterWords input = filter (all isDigit) $ words $ input
readInts input = map read $ filterWords input
readInts2 input = sapcer [] (words input)
	where
		spacer
