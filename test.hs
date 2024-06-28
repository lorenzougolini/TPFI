data BinTree a = Node a (BinTree a) (BinTree a) | Empty
data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a

inOrder :: BinTree a -> [a]
inOrder Empty = []
inOrder (Node a l r) = inOrder l ++ [a] ++ inOrder r
inOrder' :: BinTree' a -> [a]
inOrder' (Leaf a) = [a]
inOrder' (Node' l r) = inOrder' l ++ inOrder' r


foldrBT :: (a -> b -> b) -> b -> BinTree a -> b
foldrBT f v Empty = v
foldrBT f v (Node a l r) = f a (foldrBT f (foldrBT f v r) l)

-- maxPathLength :: BinTree a -> Int
-- maxPathLength tree = foldrBT searchPath (0,0) tree where
--     searchPath 
--         | (maxH, h) Empty     = (maxH, 0)
--         | otherwise           = (max maxH h+1, h+1) 

-- maxPathLength = fst $ foldrBT searchPath (0, 0)
--   where
--     searchPath :: (Int, Int) -> BinTree a -> (Int, Int)
--     searchPath _ (maxH, h) = (max maxH (h + 1), h + 1)

-- foldlBT' :: (b -> a -> b) -> b -> BinTree' a -> b
-- foldlBT' f v (Leaf a) = f v a
-- foldlBT' f v (Node' l r) = foldlBT' f (foldlBT' f v l) r

--treeHeight using foldrBT
treeHeight'' :: BinTree Integer -> Integer
treeHeight'' = foldrBT (\x _ -> 1 + x) 0






-- nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
-- nodiEquilibrati t = nodiEquilibratiAux t 0 where
--     nodiEquilibratiAux Empty _ = []
--     nodiEquilibratiAux (Node a l r) s
--         | s == a + s' = a : nodiEquilibratiAux l s' ++ nodiEquilibratiAux r s'
--         | otherwise = nodiEquilibratiAux l s' ++ nodiEquilibratiAux r s'
--         where s' = s + a

-- code to test functions
-- let tree = Node 6 (Node 2 (Node 3 Empty Empty) (Node 4 (Node 6 Empty Empty) Empty)) (Node 5 (Node 1 Empty Empty) Empty)
-- let tree' = Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Leaf 3) (Leaf 4))


-- Funzione ausiliaria che calcola la somma di un sottoalbero
sommaSottoalbero :: Num a => BinTree a -> a
sommaSottoalbero Empty = 0
sommaSottoalbero (Node v l r) = v + sommaSottoalbero l + sommaSottoalbero r

-- Funzione principale che trova i nodi equilibrati
nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
nodiEquilibrati albero = fst (aux albero 0) where
    aux :: (Num a, Eq a) => BinTree a -> a -> ([a], a)
    aux Empty _ = ([], 0)
    aux (Node v l r) sommaCammino = 
        let        -- Calcola la somma del sottoalbero radicato nel nodo attuale
        sommaSotto = sommaSottoalbero (Node v l r) -- Calcola i risultati per i sottoalberi sinistro e destro
        (equilibratiL, sommaL) = aux l (sommaCammino + v)
        (equilibratiR, sommaR) = aux r (sommaCammino + v)        -- Controlla se il nodo attuale è equilibrato
        equilibrati = ([v | sommaCammino == sommaSotto])
        in        -- Ritorna i nodi equilibrati inclusi i risultati dei sottoalberi e la somma del sottoalbero attuale
        (equilibrati ++ equilibratiL ++ equilibratiR, sommaSotto)
        
-- Esempio d'uso
main :: IO ()
main = do
    let albero = Node 6 (Node 3 Empty Empty) (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))  
    print $ nodiEquilibrati albero



---------------------------------------------------------------------

filter :: (a -> Bool) -> [a] -> [a]
filter f (x:xs) = if f x then x : filter f xs else filter f xs
