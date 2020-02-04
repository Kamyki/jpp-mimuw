mysequence :: Moand m => [m a] -> m [a]
mysequence [] = return []
mysequence (x:xs) = do 
	hd <- x;
	tail <- mysequence xs
	return (hd:tail)

myMapM :: Moand m => (a -> m b) -> [a] -> m [b]
myMapM f [] = return []
myMapM f (x:xs) = do
	hd <- f x;
	tl <- myMapM f xs
	return (hd:tl)

myFoldM :: Moand m => [a] -> (a -> m b) -> m [b]
myFoldM = flip mymapM


allComb :: [[a]] -> [[a]]
allComb [] = [[]]
allComb (l:ls) = do
	x <- l
	xs <- allComb ls
	return $ x:xs