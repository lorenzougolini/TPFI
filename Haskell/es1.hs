type SymList a = ([a], [a])
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys --funzione di astrazione

-- fromSL (consSL x sxs) = x:(fromSL sxs)
consSL x (xs, []) = ([x],xs) --se ho ys = [] allora xs ha 0 o 1 elemento
consSL x (xs, ys) = (x:xs, ys)

-- fromSL (snocSL x sxs) = fromSL sxs ++ [x]
snocSL x ([], ys) = (ys, [x])
snocSL x (xs, ys) = (xs, x:ys)

-- implementazione delle altre funzioni!! FALLE!!

headSL (x:xs, ys) = x
headSL ([], []) = undefined --non serve scriverlo
headSL ([], [y]) = y
lastSL (xs, y:ys) = y
lastSL ([x], []) = x 

tail (x:xs, ys)
    | null xs = (reverse rs, ls)
    | otherwise = (xs, ys)
where 
    (ls, rs) = splitAt ys m
    m = length ys `div` 2