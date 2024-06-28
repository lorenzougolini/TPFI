-- 1. mergeSort “iterativo”

-- 1.1 Definire una funzione Haskell che segue la seguente idea bottom-up per
-- implementare l’algoritmo mergeSort:
-- Data una lista xs, creare una lista di liste lunghe 1, ciascuna contenente un elemento di xs, 
-- poi fondere a due a due le liste ordinate (eventualmente lasciando inalterata l’ultima lista
-- quando il numero delle liste è dispari), finchè non rimane un’unica lista ordinata.
-- Ad esempio, cominciando con [5,3,4,2,1] la funzione calcola le seguenti
-- liste: [[5],[3],[4],[2],[1]], poi [[3,5],[2,4],[1]], [[2,3,4,5],[1]] e
-- infine [[1,2,3,4,5]] da cui viene estratto il risultato finale [1,2,3,4,5].
mergeSortIterative :: Ord a => [a] -> [a]
mergeSortIterative = head . mergePairs . map(:[]) 

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

mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs [] = []
mergePairs [x] = [x]
mergePairs xs = map mergeSort (mergePairs (mergePairsAux xs)) where
    mergePairsAux [] = []
    mergePairsAux [x] = [x]
    mergePairsAux (x:y:xs) = (x ++ y) : mergePairsAux xs

-- 1.2 Accelerare la prima fase di questo algoritmo per trarre vantaggio da
-- input “favorevoli”. La migliorìa dovrebbe assicurare un comportamento lineare
-- in casi particolarmente fortunati
mergeSortIterative' :: Ord a => [a] -> [a]
mergeSortIterative' = head . mergePairs . firstStep

firstStep :: Ord a => [a] -> [[a]]
firstStep [] = []
firstStep [x] = [[x]]
firstStep xs@(x:xss) = segment : firstStep rest where
    (segment, rest) = span (>= x) xs
-- tiene gli elementi già ordinati in segmenti unici


-- 2. Alberi & funzionali sugli alberi
-- Considerare le seguenti definizione di alberi binari:
-- data BinTree a = Node a (Bintree a) (BinTree a) | Empty
-- data BinTree’ a = Node’ (BinTree’ a) (BinTree’ a) | Leaf a

-- 2.1 Scrivere i funzionali mapBT, mapBT’, foldrBT, foldrBT’, foldlBT, e
-- foldlBT’ che generalizzano agli alberi BinTree e BinTree’ gli analoghi funzio-
-- nali map, foldr e foldl sulle liste. Riflettete accuratamente sui tipi che devono
-- avere e su quali siano, di fatto, i principi di ricorsione sugli alberi binari.

data BinTree a = Node a (BinTree a) (BinTree a) | Empty

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f Empty = Empty
mapBT f (Node a l r) = Node (f a) (mapBT f l) (mapBT f r)

foldrBT :: (a -> b -> b) -> b -> BinTree a -> b
foldrBT f v Empty = v
foldrBT f v (Node a l r) = f a (foldrBT f (foldrBT f v r) l)

foldlBT :: (b -> a -> b) -> b -> BinTree a -> b
foldlBT f v Empty = v
foldlBT f v (Node a l r) = f (foldlBT f (foldlBT f v l) r) a


data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a

mapBT' :: (a -> b) -> BinTree' a -> BinTree' b
mapBT' f (Leaf a) = Leaf (f a)
mapBT' f (Node' l r) = Node' (mapBT' f l) (mapBT' f r)

foldrBT' :: (a -> b -> b) -> b -> BinTree' a -> b
foldrBT' f v (Leaf a) = f a v
foldrBT' f v (Node' l r) = foldrBT' f (foldrBT' f v r) l

foldlBT' :: (b -> a -> b) -> b -> BinTree' a -> b
foldlBT' f v (Leaf a) = f v a
foldlBT' f v (Node' l r) = foldlBT' f (foldlBT' f v l) r

-- 2.2 Scrivere poi le seguenti funzioni usando foldrBT e foldrBT’ (cercare di
-- ottenere algoritmi lineari nel numero dei nodi):
-- (a) numero dei nodi di un albero binario;

countNodes :: BinTree a -> Int
countNodes = foldrBT (\x->(+1)) 0

-- non mi è chiaro se i nodi senza valore debbano essere contati, 
-- dal momento che applicando funzioni (per esempio con map) non sarebbero considerati 
-- ma solo usati come 'appoggio' per raggiungere le Leaf
countNodes' :: BinTree' a -> Int
countNodes' = foldrBT' (\x->(+1)) 0

-- (b) altezza dell’albero (= lunghezza in numero di archi del più lungo cam-
-- mino radice-foglia)

------------funzione che usa ricorsione senza foldrBT-------------
-- treeHeight :: BinTree a -> Int
-- treeHeight Empty = 0
-- treeHeight (Node _ Empty Empty) = 0
-- treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)
------------------------------------------------------------------

-- ho dovuto definire un altra funzione foldrBT per analizzare i nodi nel completo
treeDepth :: (Num a, Ord a) => BinTree a -> a
treeDepth tree = fst (foldrBTAux contaSubNodes (0,0) tree) where
    foldrBTAux f v Empty = v
    foldrBTAux f v (Node a l r) = max (f (Node a l r) (foldrBTAux f (foldrBTAux f v r) l)) (f (Node a l r) (foldrBTAux f (foldrBTAux f v l) r))

-- non credo sia correttissimo l'uso di max tra i valori risultanti da foldrBT sul sotto-albero sinistro e destro,
-- ma è l'unica soluzione trovata corretta che usa foldrBT e non semplice ricorsione
contaSubNodes :: (Num a, Ord a) => BinTree a -> (a, a) -> (a, a)
contaSubNodes (Node _ Empty Empty) (maxH, h) = if h > maxH then (h, 0) else (maxH, 0)
contaSubNodes (Node _ Empty _) (maxH, h) = (maxH, h + 1)
contaSubNodes (Node _ _ Empty) (maxH, h) = (maxH, h + 1)
contaSubNodes (Node _ l r) (maxH, h) = (maxH, h + 2)

-- (c) massimo indice di sbilanciamento (= massima differenza tra altezza
-- sotto-albero destro/sinistro)

maxSbilanciamento :: (Num a, Ord a) => BinTree a -> a
maxSbilanciamento tree = fst (foldrBTAux' sbilanciamentoNodo (0,0) tree)

foldrBTAux' :: (Num a, Ord a) => (BinTree a -> (a, a) -> (a, a)) -> (a, a) -> BinTree a -> (a, a)
foldrBTAux' f v Empty = v
foldrBTAux' f v (Node a l r) = (max maxSbil (abs (hR - hL)), 1 + max hL hR) where
    (maxSbil, height) = f (Node a l r) (sbilL, sbilR)
    (sbilL, hL) = foldrBTAux' f v l
    (sbilR, hR) = foldrBTAux' f v r

sbilanciamentoNodo :: (Num a, Ord a) => BinTree a -> (a, a) -> (a, a)
sbilanciamentoNodo (Node _ Empty Empty) (sbilL, sbilR) = (sbilL, sbilR)
sbilanciamentoNodo (Node _ Empty _) (sbilL, sbilR) = (sbilL, sbilR + 1)
sbilanciamentoNodo (Node _ _ Empty) (sbilL, sbilR) = (sbilL + 1, sbilR)
sbilanciamentoNodo (Node _ l r) (sbilL, sbilR) = (sbilL, sbilR)

-- Facoltativo: Gli alberi a branching illimitato si possono facilmente de-
-- finire in Haskell come segue: data Tree a = R a [Tree a]. Come ai punti
-- precedenti, scrivendo i funzionali mapT, foldrT e foldlT.

data Tree a = R a [Tree a]

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (R a ts) = R (f a) (map (mapT f) ts)

foldrT :: (a -> b -> b) -> b -> Tree a -> b
foldrT f v (R a ts) = f a (foldrT' f v ts) where
    foldrT' f v [] = v
    foldrT' f v (x:xs) = foldrT f (foldrT' f v xs) x

foldlT :: (b -> a -> b) -> b -> Tree a -> b
foldlT f v (R a ts) = foldlT' f (f v a) ts where
    foldlT' f v [] = v
    foldlT' f v (x:xs) = foldlT f (foldlT' f v xs) x


-- 3. Nodi Equilibrati
-- Un nodo u di un albero (considerare a piacere i BinTree oppure i Tree dell’esercizio precedente) 
-- con valori numerici in tutti i nodi `e detto equilibrato se la somma delle chiavi 
-- nel cammino dalla radice a u (esclusa la chiave in u) è esattamente uguale alla somma
-- delle chiavi del sotto-albero radicato in u (compresa la chiave in u).
-- Scrivere una funzione nodiEquilibrati :: Num a => BinTree a -> [a] che preso
-- in input un albero, restituisce la lista (eventualmente vuota) contenente tutti
-- i valori nei nodi equilibrati.
-- Valutare la complessità della funzione.

nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
nodiEquilibrati tree = fst (nodiEquilibratiAux tree 0) where
    nodiEquilibratiAux Empty _ = ([], 0)
    nodiEquilibratiAux (Node u l r) sumToU = (nodiTrovati ++ nodiEquilibratiL ++ nodiEquilibratiR, sommaSottoU) where
        sommaSottoU = sumFromU (Node u l r)
        nodiTrovati = [u | sumToU == sommaSottoU]
        (nodiEquilibratiL, sommaL) = nodiEquilibratiAux l (sumToU + u)
        (nodiEquilibratiR, sommaR) = nodiEquilibratiAux r (sumToU + u)

sumFromU :: Num a => BinTree a -> a
sumFromU Empty = 0
sumFromU (Node v l r) = v + sumFromU l + sumFromU r

-- ogni nodo dell'albero viene visitato una volta da nodiEquilibratiAux: O(n)
-- sumFromU ha un massimo di n visie da fare (la somma del sotto-albero alla root): O(n)
-- complessità totale è O(n)


-- 4. Alberi Binari di Ricerca
-- Scrivere una funzione Haskell listToABR :: Ord a ⇒[a] →BinTree a che
-- sistema i valori di una lista in un albero binario di ricerca.
-- Determinare la complessit`a della funzione e chiedersi se si tratta di una
-- complessit`a ottima rispetto al problema

listToABR :: Ord a => [a] -> BinTree a
listToABR xs = buildABR (mergeSort xs, length xs) where
    buildABR ([], _) = Empty
    buildABR ([x], _) = Node x Empty Empty
    buildABR (xs, n) = Node root (buildABR (left, k)) (buildABR (right, n-k)) where
        k = n `div` 2
        (left, root:right) = splitAt k xs

-- la complessità della funzione è O(nlogn)
-- si possono ridurre passaggi ridondanti come il calcolo della lunghezza con il tupling, passandolo come parametro


-- 5. Derivazioni di programmi
-- La funzione scanr :: (a → b) → b → [a] → [b] può essere facilmente definita
-- componendo map, foldr e tails (chiamata suffissi nell’Homework precedente):
-- scanr f e = map (foldr f e) . tails
-- Usare la definizione sopra come specifica per derivare una definizione efficiente
-- (cioè lineare nella lunghezza della lista) facendo manipolazioni algebriche, in
-- analogia con quanto visto per scanl (slide lezione 9).

-- definizione di tails:
-- tails :: [a] -> [[a]]
-- tails [] = [[]]
-- tails xs@(_:txs) = xs : tails txs

-- caso base: xs = []
-- scanr f e [] =
--     = map (foldr f e) (tails []) =
--     = map (foldr f e) [[]] = 
--     = [foldr f e []] = 
--     = [e]

-- caso induttivo: xs = x:xs
-- scanr f e (x:xs) =
--     = map (foldr f e) (tails (x:xs)) =
--     = map (foldr f e) ((x:xs) : tails xs) =
--     = foldr f e (x:xs) : map (foldr f e) (tails xs) =
--     = f x (foldr f e xs) : scanr f e xs = 
--     = f x (head (scanr f e xs)) : scanr f e xs =

-- quindi la definizione lineare di scanr è:
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- scanr f e [] = [e]
-- scanr f e (x:xs) = f x (head ys) : ys where
--     ys = scanr f e xs
