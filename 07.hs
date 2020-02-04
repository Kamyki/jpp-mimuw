{-
ReaderT r Identity a
r -> m a

ReaderT r (StateT s Indentity) a
r -> s -> Identity (a, s)

ReaderT r (StateT s (ExceptT String Identity)) a
r -> s -> (Either String (a, s))


StateT r (ReaderT Identity) a
s -> r -> (a,s)

ExceptT e (StateT s (ReaderT r Identity)) a
s -> r -> m3 (Either e, (am s))

-}

import Control.Monad.Reader
import Control.Monad.State

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

type MMonad s = ReaderT Int (State Int) s

numberTree :: Tree Bool -> Tree Int
numberTree t = evalState (runReaderT (numberTree_ t) 0) 100


numberTree_ :: Tree Bool -> MMonad (Tree Int)
numberTree_ Empty = return Empty
numberTree_ (Node x l r) = do
	l' <- local (+1) (numberTree_ l)
	
	i <- if x then get else ask
	modify (+1)

	r' <- local (+1) (numberTree_ r)
	return $ Node i l' r'

