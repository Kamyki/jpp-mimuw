import Prelude hiding(Either(..))

data Either a b = Left a | Right b deriving (Show)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

instance Functor (Either e) where
    fmap f (Right a) = Right (f a)
    fmap f (Left a)  = Left a


reverseRight :: Either e [a] -> Either e [a]
reverseRight (Left e) = Left e
reverseRight (Right x) =  Right (reverse x)

reverseRight2 :: Either e [a] -> Either e [a]
reverseRight2 x = fmap reverse x


class Functor f => Pointed f where
  pure :: a -> f a

instance Pointed (Maybe) where
    pure x = Just x

instance Pointed [] where
    pure x = [x]

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Pointed (Tree) where
    pure x = Node x Empty Empty