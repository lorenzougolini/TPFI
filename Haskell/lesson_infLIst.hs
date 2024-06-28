-- 11
nats = nextNat 0 where
    nextNat n = n:nextNat (n+1)

nats' = 0 : map (+1) nats'

powers' n = ps where ps = 1:map (n*) ps

factors n = [x | x <- takeWhile(<n)[1..], n `mod` x == 0]
primes = [p | p <- [2..], factors p == [1,p]]

-- è una funzione circolare
myRepeat x = xs where xs = x:xs

-- iterate non circolare
iterate1 f x = x : iterate1 f (f x)
-- iterate lineare
iterate2 f x = xs where xs = x:map f xs
-- iterate quadratica 
iterate3 f x = x : map f (iterate3 f x)


-- 13
iterate' :: (t -> t) -> t -> [t]
iterate' f x = x : iterate' f (f x)

unfold :: (b -> (a,b)) -> b -> [a] 
unfold f y = x : unfold f y' where (x, y') = f y

unfoldLS :: (b -> Bool) -> (b -> (a,b)) -> b -> [a]
unfoldLS p f y = if p y then []
                else x : unfoldLS p f y' where (x,y') = f y

map' f xs = unfoldLS null (f . head) tail

nextH (hp:hps) (hq:hqs)
    | hp < hq   = hp:nextH hps (hq:hqs)
    | hq < hp   = hq:nextH (hp:hps) hqs
    | otherwise = hp:nextH hps hqs

hamming p q = hs where hs = 1:nextH (map (p*) hs) (map (q*) hs)

nextH' xs = m:nextH' ys where
    m = foldr1 min (map head xs)
    ys = map (\zs@(z:tzs) -> if z==m then tzs else zs) xs
hammings' gs = hs where hs = 1:nextH' map (\x -> (x*) hs) gs

primes' :: [a]
primes' = filterP [2..] where
    filterP (p:ps) = p : filterP [x | x <- ps, x `mod` p /= 0]

primes'' = 2:[x | x <- [3..], isPrime x] where
    isPrime x = all (\p -> x `mod` p /= 0) (factorsToTry x)
    factorsToTry x = takeWhile (\p -> p*p <= x) primes

-- 14
multipleOf n = map (n*) [1..]

allMultiples :: [[a]]
allMultiples = [ map (n*) [1..] | n <- [2..]]


unionP :: Ord a => [a] -> [a] -> [a]
unionP (x:xs) ys = x:union xs ys

union :: Ord a => [a] -> [a] -> [a]
union xs@(x:txs) ys@(y:tys)
    | x < y     = x:union txs ys
    | x > y     = y:union xs tys
    | otherwise = x:union txs tys
unionAll:: Ord a => [[a]] -> [a]
unionAll = foldr1 union

-- definire primi come naturali\\composti
-- composti sono unione di multipli di primi
minus xs@(x:txs) ys@(y:tys)
    | x < y     = x : minus txs ys
    | y < x     = y : minus xs tys
    | otherwise = x : minus xs ys

primes''' = [2..] `minus` composites where
    composites = unionAll [map (p*) [p..] | p <- primes]
-- this doesn't work, unionAll non produce mai il primo numero:
-- impossibile sapere qual è il primo elemento della computazione [2..] minus composites
-- ma si sa essere 2, quindi:
unionAll' = foldr1 unionP
primes'''' = 2:[3..] `minus` composites where
    composites = unionAll' [map (p*) [p..] | p <- primes]
