-- 1. Input/Output
-- Definite un’azione charCount :: IO () che legge un numero n da tastiera,
-- poi n stringhe e alla fine stampa il numero di stringhe in cui appare ciascuna
-- lettera

charCount:: IO ()
charCount = do
    putStrLn "Insert a number: "
    n <- getStringsNumber
    putStrLn ("Insert " ++ show n ++ " strings: ")
    strs <- getNStrings n
    let res = countLetterPresence strs
    putStrLn ("Char count:" ++ show res)

getStringsNumber :: IO Int
getStringsNumber = do 
    n <- getLine
    return (read n :: Int)

getNStrings :: Int -> IO [String]
getNStrings 0 = return []
getNStrings n = do 
    x <- getLine
    rest <- getNStrings (n-1)
    return (x:rest)

countLetterPresence :: [String] -> [(Char, Int)]
countLetterPresence xs = map (\x -> (x, countCharPresence x xs)) ['a'..'z'] where
    countCharPresence c xs = length (filter (elem c) xs)


-- 2. Nodi Equilibrati con Applicativi e Monadi
-- Risolvere l’esercizio 3 dell’Homework 2 (Nodi Equilibrati) usando applicativi
-- e monadi (in analogia con le funzioni che creano un albero o che rietichettano
-- i nodi dei un albero visti nella Lezione ), in modo da evitare di dover usare
-- (esplicitamente nel codice) parametri e risultati di ritorno ausiliari.
--
-- mia soluzione homework 2
-- nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
-- nodiEquilibrati tree = fst (nodiEquilibratiAux tree 0) where
--     nodiEquilibratiAux Empty _ = ([], 0)
--     nodiEquilibratiAux (Node u l r) sumToU = (nodiTrovati ++ nodiEquilibratiL ++ nodiEquilibratiR, sommaSottoU) where
--         sommaSottoU = sumFromU (Node u l r)
--         nodiTrovati = [u | sumToU == sommaSottoU]
--         (nodiEquilibratiL, sommaL) = nodiEquilibratiAux l (sumToU + u)
--         (nodiEquilibratiR, sommaR) = nodiEquilibratiAux r (sumToU + u)
-- 
-- sumFromU :: Num a => BinTree a -> a
-- sumFromU Empty = 0
-- sumFromU (Node v l r) = v + sumFromU l + sumFromU r

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    fmap f st = S (\s -> let (x, s') = app st s in
        (f x, s'))

instance Applicative ST where
    pure x = S (\s -> (x, s))
    stf <*> stx = S (\s -> let (f, s') = app stf s in
        let (x, s'') = app stx s' in
            (f x, s''))

instance Monad ST where
    return = pure
    st >>= f = S (\s -> let (x, s') = app st s in
        app (f x) s')

data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving Show

newtype ST a = S (State -> (a, State))
type State = (Int, [Int])

sumFromU :: Num a => BinTree a -> a
sumFromU Empty = 0
sumFromU (Node u l r) = u + sumFromU l + sumFromU r

nodiEquilibrati :: BinTree Int -> [Int]
nodiEquilibrati tree = fst (app (nodiEquilibrati' tree) (0, []))

nodiEquilibrati' :: BinTree Int -> ST [Int]
nodiEquilibrati' Empty = return []
nodiEquilibrati' (Node u l r) = do
    (sumSoFar, passedNodes) <- S (\s -> (s, (fst s + u, snd s ++ [u])))
    let sumSubTree = sumFromU (Node u l r)
    let nodoTrovato = [u | sumSoFar == sumSubTree]
    nodiSx <- nodiEquilibrati' l
    nodiDx <- nodiEquilibrati' r
    S (\s -> (s, (fst s - (head (reverse (snd s))), init (snd s))))
    return (nodoTrovato ++ nodiSx ++ nodiDx)

-- tree = (Node 7 (Node 3 (Node 3 Empty Empty) (Node 1 Empty Empty)) (Node 6 (Node 1 Empty Empty) (Node 9 (Node 3 Empty Empty) (Node 1 Empty Empty))))
-- nodiEquilibrati tree = [3,9]


-- 3. Monadi/Eccezioni
-- Definire il tipo NatBin che rappresenta i numeri naturali come sequenze binarie.
-- Potete definirlo come liste (di lunghezza fissata) di 0 e 1, oppure potete dare
-- una definizione con data (ad esempio usando 3 costruttori, di cui uno sia la
-- costante 0 e gli altri due... in ogni caso, immaginare di definire una “parola di
-- memoria”, quindi prevedete una lunghezza massima costante).
-- Definire un valutatore di espressioni aritmetiche su NatBin, analoghi a quel-
-- li visti a lezione, ma considerare tutte le operazioni aritmetiche (+, ×, div, mod
-- e -). Estendere il tipo Maybe in modo che il risultato di un’espressione possa
-- essere eventualmente un’eccezione diversa a seconda dell’eventuale situazio-
-- ne anomala che si `e verificata: divisione per zero, numero negativo oppure
-- overflow.
-- Potete completare l’esercizio facendo in modo che il tipo NatBin sia un’i-
-- stanza delle usuali classi Eq, Ord, Num, Show

-- data Digit = Zero | One 
-- type NatBin = [Digit]

type NatBin = [Int]

data Op = Const NatBin 
            | Sum Op Op
            | Sub Op Op 
            | Mul Op Op
            | Div Op Op
            | Mod Op Op

checkBinWord :: NatBin -> Maybe NatBin
checkBinWord n = if length n > 8 && head n == 1 then Nothing else Just n 

intToNatBin :: Int -> Maybe NatBin
intToNatBin 0 = Just [0] 
intToNatBin n = do
    if n < 0 then Nothing else 
        case checkBinWord (reverse (intToNatBinAux n)) of
            Nothing -> Nothing
            Just x -> Just x 
            where
                intToNatBinAux 0 = []
                intToNatBinAux n = let (q,r) = n `divMod` 2 in r : intToNatBinAux q

natBinToInt :: NatBin -> Maybe Int
natBinToInt n = if length n == 8 && head n == 1 then Nothing else 
    Just (foldl (\acc x -> acc * 2 + x) 0 n)

eval :: Op -> Maybe NatBin
eval (Const a) = case natBinToInt a of
    Nothing -> Nothing
    Just _  -> Just a

eval (Sum a b) =
    eval a >>= \x ->
    eval b >>= \y ->
    natBinToInt x >>= \xInt ->
    natBinToInt y >>= \yInt ->
    intToNatBin (xInt + yInt)

eval (Sub a b) =
    eval a >>= \x ->
    eval b >>= \y ->
    natBinToInt x >>= \xInt ->
    natBinToInt y >>= \yInt ->
    let diff = xInt - yInt in
        if diff < 0 then Nothing else intToNatBin diff

eval (Mul a b) =
    eval a >>= \x ->
    eval b >>= \y ->
    natBinToInt x >>= \xInt ->
    natBinToInt y >>= \yInt ->
    intToNatBin (xInt * yInt)

eval (Div a b) =
    eval a >>= \x ->
    eval b >>= \y ->
    natBinToInt x >>= \xInt ->
    natBinToInt y >>= \yInt ->
    if yInt == 0 then Nothing else intToNatBin (xInt `div` yInt)

eval (Mod a b) =
    eval a >>= \x ->
    eval b >>= \y ->
    natBinToInt x >>= \xInt ->
    natBinToInt y >>= \yInt ->
    if yInt == 0 then Nothing else intToNatBin (xInt `mod` yInt)