myNot :: Bool -> Bool
myNot x
    | x = False
    | otherwise = True

myNot' :: Bool -> Bool
myNot' False = True
myNot' _ = False

xor :: Eq a => a -> a -> Bool
xor x y
    | x /= y = True
    | otherwise = False

fatt 0 = 1
fatt n = n * fatt (n-1)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

myFst (x, _) = x
mySnd (_, y) = y
mySum (x, y) = x + y

fibAux 0 = (0, 0)
fibAux 1 = (1, 0)
fibAux n = (f + fPrec, f) where (f, fPrec) = fibAux (n-1)
fib' n = fst (fibAux n)

testa (x:_) = x
coda (_:xs) = xs
ultimo [x] = x
ultimo (_:xs) = ultimo xs 

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

palindrome xs = xs == reverse xs

merge [] ys = ys
merge xs [] = xs
merge xs@(x:txs) ys@(y:tys)
    | x <= y    = x:merge txs ys
    | otherwise = y:merge xs tys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSort ls `merge` mergeSort rs 
    where (ls, rs) = splitAt (length xs `div` 2) xs

inits :: [a] -> [[a]]
inits (x:xs) = [] : map (x:) (inits xs)
inits [] = [[]]

suffissi :: [a] -> [[a]]
suffissi [] = [[]]
suffissi xs = xs : suffissi (tail xs)

myConcat (xs:xss) = xs ++ myConcat xss
myConcat [] = []

myConcat' xss = foldr (++) [] xss
