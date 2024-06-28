first :: [(a, b)] -> [a]
first ps = [x | (x,_) <- ps]

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

myMapLC f xs = [f x | x <- xs]
myFilterLC p xs = [x | x <- xs, p x]

myConcat xss = [x | xs <- xss, x <- xs]

ps1 xs ys = sum (map ( uncurry (*)) (zip xs ys))

-- data lista di funzioni e lista di argomenti,
-- applica ordinatamente la prima alla seconda
applyL (f:fs)(x:xs) = f x : applyL fs xs
applyL _ _ = []
ps2 xs ys = sum (applyL (map (*) xs) ys)

ps3 xs ys = sum (zipWith (*) xs ys)

myAny p = or . map p
myAll p = and . map p

-- ritorna posizione elemento v, else -1
find v xs = findAux v xs 0
findAux v (x:xs) n
    | v == x    = n
    | otherwise = findAux v xs (n+1)
findAux x [] _ = -1

findVPH v xs =
    (fst . head) (
        filter (\(_, y) -> y == v)
        (zip [0..] xs) ++ [(-1, v)])

-- returna comunque -1 anche senza appenderlo alla fine
findVPH' v xs =(fst . head) (filter (\(x, y) -> v==y) (zip [0..] xs))

-- foldr inietta una funzione binaria in una lista
myFoldr f v (x:xs) = f x (myFoldr f v xs)
myFoldr f v [] = v

-- foldr1 inserisce il valore minimo
myFoldr1 f [x] = x
myFoldr1 f (x:xs ) = f x (myFoldr1 f xs)

-- controlla una lista è ordinata
ordinata [] = True
ordinata [x] = True
ordinata (x:y:xs) = x <= y && ordinata (y:xs)

ordinataVPH xs = foldr (&&) True (zipWith (<=) xs (tail xs))
ordinataVPH' xs = and (zipWith (<=) xs (tail xs))

-- prodotto scalare in matrici
tranpose [] = []
transpose [xs] = map (: []) xs
transpose (xs:xss) = zipWith (:) xs (transpose xss)

prodMat a b = map (\x -> map (ps1 x) (transpose b)) a