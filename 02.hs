ones  = 1 : ones

nats 0 : (map (uncurry (+)) $ zip ones nats)

nats1  = 0 : zipWith (+) ones nats1

fact1 = 1 : zipWith (*) [1..] fact1

fact = (:) 1 $ map star $ zip [1..] fact where star = uncurry (*)

primes :: [Integer]

primes = sieve [2..] where
  sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]


positions :: Char -> String -> [Int]
positions c s = [i | (d,i) <- zip s [0..], c == d]

indexOf c s =
  case positions c s of
  [] -> Nothing
  x:_ -> Just x

indexOf' :: Char -> String -> Maybe Int
indexOf' c [] = Nohing
indexOf' c l = licznik 0 l
  where
    licznik i [] = NOthing
    licznik i (x:xs) = if c == x then Just i licznik (i+1) xs

positions2 :: Char -> String -> [Int]
positions2 c [] = []
positions2 c l = licznik 0 l
  where
    licznik i [] = []
    licznik i (x:xs) = let l = licznik (i+1) xs in
                          if x == c then i:l else l

digit n = iterate succ '0' !! n
digit2 d = chr $ ord '0' + d

si n s
  | n == 0    = s
  | otherwise = si (n `div` 10) (digit2 (n `mod` 10) : s)

showInt 0 = '0'
showInt n = si n []


showIntListContent [] = ""
showIntListContent ([n]) = showInt n
showIntListContent (n:ns) = show Int n ++ ", " ++ showIntListContent ns

showIntList ns  = "[" ++ showIntListContent ns ++ "]"


showLst :: (a -> String) -> [a] -> String
showLst f xs =  "[" ++ shwoLstContent xs ++ "]"
  where
    showLstContent [] = []
    showLstContent [x] = f x
    showLstContent (x:xs) = f x ++ ", " ++ (showLstContent xs)


showLst2 :: Show a => [a] -> String
showLst2 = showLst show

showIntList2 :: [Int] -> String
showIntList2 = showLst2


incAll [] = []
incAll (x:xs) = let
  zwieksz [] = []
  zwieksz (y:ys) = (y+1):(zwieksz ys)
  in (zwieksz x):(incAll xs)

incAll1 ll = let
  zwieksz l = map (\x -> x+1) l
  in map zwieksz ll

incAll2 ll = map (map (\x -> x+1)) ll

incAll3 l = map (map (+1)) l

incAll4 xss = [ [x+1 | x<-xs] | xs<-xss]

incAll5 = map $ map (+1)

-- mało efektywne - musi rozwinąć całą listę, a jest leniwy wiec rzadko kiedy ją zwinie do wyniku
silniaFoldl n = foldl (*) 1 [1..n]
-- ((0!*1)*2)*3)*4)*5

-- jest eager, zawsze zwinie wyrażenie oszczędzając pamięć
silniaFoldl' n = foldl' (*) 1 [1..n]


concat1 :: [[a]] -> [a]
concat1 = foldr (++) []

concat2 :: [[a]] -> [a]
concat2 = foldl (++) [] -- lenght $ concat2 [ [n..n+2] | n <- [0..8000 ] -- ma czas O(n^2)

-- seq:: a -> b -> b
--  obliczt to, zwróć to


silniaFoldr n = foldr (*) 1 [1...n]
-- (1*(2*(3*(4*(5*0!))))




