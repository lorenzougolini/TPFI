import Data.List (sortBy)
-- 1.1 Dare la definizione di myTakeWhile e myDropWhile;
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs) = if f x then x : myTakeWhile f xs else []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f (x:xs) = if f x then myDropWhile f xs else x:xs


-- 1.2 scrivere una funzione ricorsiva myRemoveDupsOrd che rimuove i duplicati
-- da una lista ordinata xs di lunghezza n in tempo O(n).
myRemoveDupsOrd :: Ord a => [a] -> [a]
myRemoveDupsOrd [] = []
myRemoveDupsOrd [x] = [x]
myRemoveDupsOrd (x:y:xs)
    | x == y    = myRemoveDupsOrd (y:xs)
    | otherwise = x: myRemoveDupsOrd (y:xs)


-- 1.3 scrivere una funzione myRemoveDups che rimuove i duplicati da una
-- qualsiasi lista xs di lunghezza n in tempo O(n log n), preservando l’ordine
-- originale delle prime occorrenze degli elementi rimasti.
-- myRemoveDups [5,2,1,2,5,7,2,1,2,7] = [5,2,1,7]
-- myRemoveDups (x:xs) = x : myRemoveDups (filter (/=x) xs) -- NO O(n^2)

sortTuplesBy :: Ord a => (t -> a) -> [t] -> [t]
sortTuplesBy f = sortBy (\x y -> compare (f x) (f y))

myRemoveDups :: Ord b => [b] -> [b]
myRemoveDups xs = map fst (sortTuplesBy snd (removeDupAux (sortTuplesBy fst (zip xs [0..])))) where
    removeDupAux [] = []
    removeDupAux [x] = [x]
    removeDupAux (x:y:xs)
        | fst x == fst y = removeDupAux (x:xs)
        | otherwise = x : removeDupAux (y:xs)
-- running time: O(n log n) + O(n) + O(n log n) + O(n) = O(n log n)


-- ############################################################################


-- 2.1 Definire il funzionale zipWith f xs ys senza decomporre liste, ma usando
-- un’espressione che contenga zapp, f ed eventualmente xs e ys.
zapp :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zapp f (x:xs) (y:ys) = f x y : zapp f xs ys
zapp f _ _ = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f = zapp f

----- correzione
zapp' (f:fs) (x:xs) (y:ys) = f x y : zapp' fs xs ys
zapp' _ _ _ = []

zipWith'' f xs ys = repeat f `zapp'` xs `zapp'` ys
-----

-- 2.2 Abbiamo visto che zipWith è più generale di zip. Tuttavia si può definire
-- zipWith f xs ys usando zip e un paio di altri funzionali visti nella Lezione 3.
zipWith'' :: (t1 -> t2 -> b) -> [t1] -> [t2] -> [b]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)


-- 2.3 Definire il funzionale map f xs senza decomporre xs, ma usando
-- un'espressione che contenga foldr, f e xs. Fare lo stesso usando foldl.
mapWithFoldr :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
mapWithFoldr f xs = foldr (\x acc -> f x : acc) [] xs

mapWithFoldl :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
mapWithFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs


-- 2.4 argomentare brevemente sul perché non sia possibile definire foldl e
-- foldr usando map:

-- foldl e foldr sono funzionali che applicano ripetutamente una funzione binaria a una lista,
-- utilizzando un accumulatore come primo argomento e un elemento della lista come secondo argomento, riducendo 
-- la lista a un singolo valore. map invece applica una funzione ad ogni elemento di una lista, ritornando la 
-- lista con i risultati delle applicazioni della funzione.


-- ############################################################################


-- 3.1 Scrivere una funzione prefissi :: [a] → [[a]] che ritorna tutti i segmenti
-- iniziali di una lista (vedi funzione suffissi nella Lezione 2).
prefissi :: [a] -> [[a]]
prefissi xs = if null xs then [] else prefissi (init xs) ++ [xs]


-- 3.2 Senza preoccuparsi dell’efficienza, ma usando i funzionali prefissi, suffissi
-- e altri funzionali dello standard Prelude, scrivere una funzione 
-- segSommaS :: (Num a) ⇒ [a] → a → [[a]] che data una lista numerica xs e un valore s
-- restituisce tutti i segmenti (cioè sottoliste di elementi consecutivi) di xs di
-- somma s.
suffissi :: [a] -> [[a]]
suffissi xs@(_:txs) = xs:suffissi txs
suffissi     []     = []

segSommaS :: (Eq a, Num a) => [a] -> a -> [[a]]
segSommaS _ 0 = [[]]
segSommaS xs s = filter (\x -> sum x == s) (concatMap prefissi (suffissi xs))


-- 3.3 Scrivere una funzione sublSommaS :: (Num a) ⇒ [a] → a → [[a]] che
-- data una lista numerica e un valore s restituisce tutte le sottoliste (anche di
-- elementi non consecutivi) di somma s.
sublSommaS :: (Eq a, Num a) => [a] -> a -> [[a]]
sublSommaS [] _ = []
sublSommaS _ 0 = [[]]
sublSommaS (x:xs) s 
    | x == s    = [x] : sublSommaS xs s                                 
    | otherwise = map (x:) (sublSommaS xs (s - x)) ++ sublSommaS xs s   
    -- modi per scrivere s usando x ++ modi per scrivere s non usando x


-- ############################################################################


-- 4.1 Scrivere una funzione Haskell part :: Int →Integer, che calcola il numero
-- di partizioni di un certo numero n. Ad esempio, part 4 calcola 5
-- part :: Int -> Integer


-- 4.2 Se invece considero diverse tra loro anche partizioni che differiscono solo
-- per l’ordine, quante sono?
part' :: Int -> Integer
part' n = toInteger (length (allParts n)) where
    allParts 0 = [[]]
    allParts n = [x:xs | x <- [1..n], xs <- allParts (n - x)]

-- 4.3 Scrivere poi una funzione Haskell parts :: Int → [[Int]]. che calcola la
-- lista delle partizioni di n. Ad esempio, parts 4 calcola la lista [[1,1,1,1],
-- [1,1,2], [1,3], [2,2], [4]] (potrebbe ovviamente essere diverso l’ordine
-- in cui si scrivono, ma suggerisco di seguire un ordine tanto nella generazione
-- che nel conteggio)
parts :: Int -> [[Int]]
parts n = findParts n n where 
    -- n = numero da partizionare, m = massimo numero usabile. init: m = n
    findParts n m
        | n == 0            = [[]]
        | m == 0            = []
        | n < m             = findParts n n
        | otherwise         = findParts n (m - 1) ++ map (m:) (findParts (n - m) m) 
        -- modi per scrivere n non usando m ++ modi per scrivere n usando m
        
-- 4.4 (Facile) Ma scrivere part usando parts? E la complessità è molto
-- maggiore della part originaria?
part :: Int -> Integer
part n = toInteger (length (parts n))