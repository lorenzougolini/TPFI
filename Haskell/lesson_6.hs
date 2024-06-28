expon m 0 = 1
expon m 1 = m
expon m n
    | r == 0    = es * es 
    | otherwise = m * expon m (n-1) 
    where 
        (q, r) = divMod n 2
        es = expon m q


mySum :: Num a => [a] -> a
mySum = foldr (+) 0
myLength :: Num a => [b] -> a
myLength = foldr (\x->(+1)) 0
