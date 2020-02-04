:{
mhead [] = undefined
mhead (h:t) = h
:}

:{
mtail [] = []
mtail (h:t) = t
:}

:{
let mplus [] b = b
	mplus (h:t) b = h : (mplus t b)
:}

:{
let mtake n [] = []
	mtake 0 a = []
	mtake n (h:t) = h : (mtake (n-1) t)	
:}

:{
	
:}

:{
let mdrop n [] = []
	mdrop 0 a = a
	mdrop n (h:t) = mdrop (n-1) t	
:}

:{
mydrop n [] = []
mydrop n l@(x:xs)
  | n == 0 = l
  | otherwise = mydrop (n-1) xs	
:}

:{
mydrop n [] = []
mydrop n (x:xs)
  | n == 0 = (x:xs)
  | otherwise = mydrop (n-1) xs	
:}

:{
mfilter f [] = []
mfilter f l = [x | x <- l, f x == True] 	
:}

:{
mmap f [] = []
mmap f (h:t) = (f h):(mmap f t)	
:}

inits l = [x | x <- l, a <- h]


rec (ha:ta) (h:t) = rec (h:ha):ha:ta t  
:{
inits :: [a] -> [[a]]
inits [] = [[]]
inits a = reverse (rec [[]] a)
  where rec acc [] = acc
        rec acc@(ha:ta) (h:t) = rec ((ha ++ [h]):acc) t
:}

:{
let inits :: [a] -> [[a]]
    inits [] = [[]]
    intis (x:xs) = []:(map (x:) (inits xs))
:}

partitions l = [[x,y] |x <- inits l, y<-map reverse (inits (reverse l)), x ++ y == l]

:{
partitions a = rec [] a
  where rec acc [] = acc
        rec acc (h:t) = ([[h], t]:[h:x| x <-acc]) t
:}

:{
myreverse [] = []
myreverse (h:t) = (myreverse t) ++ [h]	
:}