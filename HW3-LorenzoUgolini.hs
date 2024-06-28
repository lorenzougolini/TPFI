-- 1. Insomnia
-- Scrivere una funzione Haskell che genera la lista infinita di caratteri 
-- insonnia = "1 sheep 2 sheep 3 sheep 4 sheep ...". Provare a scrivere un “one-
-- liner”, cio`e un programma che semplicemente compone opportunamente fun-
-- zioni. Può essere utile la funzione show :: Show a => a => String che
-- trasforma un elemento di qualsiasi tipo che implementa la classe Show in una
-- stringa, cioè una lista di caratteri
insonnia :: [Char]
insonnia = concatMap (\x -> show x ++ " sheep ") [1..]


-- 2. Triangolo di Tartaglia
-- Definite in Haskell la lista infinita di liste finite tartaglia, tale che tartaglia!!n
-- sia l’n-esima riga del triangolo di Tartaglia, e quindi tartaglia!!n!!k sia il
-- coefficiente binomiale
-- (nCk).

tartaglia :: [[Integer]]
tartaglia = [1] : map (\row -> zipWith (+) (row ++ [0]) ([0] ++ row)) tartaglia

-- oppure definizione circolare --
nextRow = map (\row -> zipWith (+) (row ++ [0]) ([0] ++ row))
tartaglia' = hs where hs = [1] : nextRow hs


-- 3. Numeri Fortunati
-- Esercizio 3 (Numeri Fortunati) I numeri fortunati, introdotti da Stani-
-- slaw Ulam, sono definiti come segue:
-- 1. Dalla sequenza dei numeri naturali (escluso lo zero) tolgo tutti i secondi
-- numeri, cioè i pari.
-- 2. il secondo numero rimasto è il 3 e quindi si tolgono tutti i terzi numeri
-- tra i sopravvissuti (5, 11, 17, . . . ).
-- 3. ora si considera il terzo numero rimasto cioè il 7 e rimuovo tutti i settimi
-- numeri (il primo è il 19) e così via, fino a ottenere tutti i numeri sopravvissuti
-- a tutte le operazioni di “filtraggio”.
-- Scrivere una funzione Haskell che genera lo stream dei numeri fortunati.

togliOgni :: Int -> [a] -> [a]
togliOgni n xs = [x | (i,x) <- zip [1..] xs, i `mod` n /= 0]

fortunati :: [Integer]
fortunati = togliOgni 7 $ togliOgni 3 $ togliOgni 2 [1..]

fortunati' :: [Int] -> [a] -> [a]
fortunati' (g:gs) xs
    | null gs       = xs
    | otherwise     = fortunati' gs (togliOgni g xs)
-- fortunati' [2,3,7] [1..] = fortunati


----------------------- Esercizi Difficili ---------------------------
-- 1D. Iterazione, Ricorsione Primitiva, Church & Ackerman
-- Una funzione f :: Nat → a è definita per ricorsione primitiva dalle funzioni
-- h :: Nat → a → Nat
-- g :: a 
-- se rispetta le equazioni:
-- f (n + 1) = h n (f n) e f 0 = g
-- 1. Scrivere in Haskell il funzionale primRec che definisce la ricorsione primitiva;

primRec :: (Integer -> a -> a) -> a -> Integer -> a
primRec h g 0 = g
primRec h g n = h (n-1) (primRec h g (n-1))

-- 2. Scrivere in Haskell il funzionale primRec′che definisce la ricorsione pri-
-- mitiva, senza fare ricorsione sui naturali, ma usando l’iterazione e le coppie,
-- cio usando il funzionale for visto a lezione (slide 12, Lezione 8)
for :: (Eq t, Num t) => (b -> b) -> t -> b -> b
for f 0 = \x -> x
for f n = f . (for f (n-1))

-- 3. Dedurne che in λ–calcolo si può facilmente definire la ricorsione primitiva
-- usando i numerali di Church (che sono essenzialmente iteratori).

-- 4. Un tradizionale risultato di computabilità, stabilisce che la funzione di
-- Ackermann (vedi slide 16, Lezione 12) non sia definibile per ricorsione pri-
-- mitiva, ma ciò vale al prim’ordine. Dare una definizione della funzione di
-- Ackermann usando il funzionale primRec.



-- 2D. Le Partizioni dell’Infinito
-- Guardate attentamente la figura nella slide 2 della Lezione 14 (titolo della sotto-lezione 14(a)).
-- Si tratta di uno streams di stream (che chiameremo allPartitions nel seguito)
-- e guardandolo attentamente potete vedere che i numeri in colore ciano sono le partizioni 
-- (vedi Homework 1) del 5, questi uniti a quelli in blu, sono le partizioni del 6.
-- Unendo i numeri verdi si ottengono quelle del 7 e con i numeri rossi otteniamo quelle dell’8.
-- 1. Scrivere un one-liner Haskell partsFromAll tale che partsFromAll n allPartitions
-- sia proprio la lista di liste che rappresenta le partizioni di n (in ordine
-- ascendente, preferibilmente).

-- 2. Scrivere un’equazione ricorsiva che genera allPartitions.
-- 3. Sviluppare qualche idea per rappresentare altre strutture combinato-
-- rie in modo analogo, tipo: tutti i sottoinsiemi (finiti) dei Naturali, tutte le
-- permutazioni (a dominio finito) dei Naturali o altre di vostro gradimento.
