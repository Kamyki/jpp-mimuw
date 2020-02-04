data WypE = A | F WypF deriving (Show)
type WypF = WypL
type WypL = [WypE]

parseF :: String -> (WypF, String)
parseE :: String -> (WypE, String)
parseL :: String -> (WypL, String)


parseE s = case (head s) of
	'a' -> (A, tail s)
	'(' -> (F (fst x), snd x)
		where x = parseF s

parseL [] = ([],[])
parse x:xs
parseL s = (((e):z), snd y)
	where 
		(e, xs) = parseE s
		y = (parseL xs)
		z = fst y

parseF ('(':xs) = let (l, ys) = parseL xs in
	case head 
parseF s = let x = parseL (tail s) in 
	case (head s) of
		'(' -> case (head $ snd x) of
			')' -> (fst x, tail $ snd x)
			otherwise -> ([],[])
		otherwise -> ([],[])


