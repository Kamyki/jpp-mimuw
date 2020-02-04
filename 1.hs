

mdrop n [] = []
mdrop 0 a = a 
mdrop n (h:t) = mdrop (n-1) t
a = [1,2,3]
mdrop a
