import Control.Monad.Reader
import Control.Monad.State

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

 

-- instance MonadReader env ((->) env) where
	-- ask :: env -> env
--	ask = \e -> e 
	-- local :: (env -> env) -> (env -> a) -> (env -> a)
--	local f g = \e -> g (f e)




 -- rozwiązanie

ren :: Tree a -> (Int -> Tree Int)
ren Empty = return Empty
ren (Node x s t) = do
	i <- ask -- tutaj?
	t' <- local (+1) (ren t)
	s' <- local (+1) (ren s)
	-- i <- a może turaj?
	return $ Node i s' t'

 -- (\i -> Node i s' t') tego unikamy

ren0 :: Tree a -> Int -> Tree Int
ren0 Empty i = Empty
ren0 (Node x s t) i =
	let s' = ren0 s ((+1) i) in
	let t' = ren0 t ((+1) i) in 
	Node i s' t'


-- instance MonadReader env (Reader env) where
--	ask = Reader $ \e -> e
--	local f g = Reader $ \e -> runReader g (f e)

ren2 :: Tree a -> (Reader Int (Tree Int))
ren2 Empty = return Empty
ren2 (Node x s t) = do
	i <- ask -- tutaj?
	t' <- local (+1) (ren2 t)
	s' <- local (+1) (ren2 s)
	-- i <- a może turaj?
	return $ Node i s' t'


-- renumber with state
{-}
type Stree = State (Tree Int)

renumberTree :: Tree a -> Tree Int

renumberTree Empty = return Empty
renumberTree (Node x t s) = do
	i <- get
	j <- put (i + 1)
	t' <- renumberTree t
	s' <- renumberTree s
	return $ Node i s' t'
-}


-- rozwiązanie
renumber :: Tree a -> Tree Int
renumber t = evalState (renumbers t) 0

renumbers :: Tree a -> State Int (Tree Int)
renumbers Empty = return Empty

renumbers (Node x l r) = do
	l' <- renumbers l
	i <- get
	put $ i+1
	r' <- renumbers r
	return $ Node i l' r'


renumbers0 :: Tree a -> Int -> (Tree Int, Int)
renumbers0 Empty i = (Empty, i)
renumbers0 (Node x l r) i0 =
	let (l', i1) = renumbers0 l i0 in
	let i2 = (i1+1) in
	let (r', i3) = renumbers0 r i3
	in (Node i2 l' r', i3)

type MojaMonada s = State Int s

renumbers2 :: Tree a -> MojaMonada (Tree Int)
renumbers2 Empty = return Empty
renumbers2 (Node x l r) = do
	l' <- renumbers2 l
	i <- gets id
	modify (+1)
	r' <- renumbers2 r
	return $ Node i l' r'