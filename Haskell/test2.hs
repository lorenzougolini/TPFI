-- Dalla sequenza dei numeri naturali (escluso lo zero) tolgo tutti i secondi
-- numeri, cioè i pari.
-- il secondo numero rimasto è il 3 e quindi si tolgono tutti i terzi numeri
-- tra i sopravvissuti (5, 11, 17, . . . ).
-- ora si considera il terzo numero rimasto cioè il 7 e rimuovo tutti i settimi
-- numeri (il primo è il 19) e così via, fino a ottenere tutti i numeri sopravvissuti
-- a tutte le operazioni di “filtraggio”.

togliOgni :: Int -> [a] -> [a]
togliOgni n xs = [x | (i,x) <- zip [1..] xs, i `mod` n /= 0]

fortunati :: [Integer]
fortunati = togliOgni 7 $ togliOgni 3 $ togliOgni 2 [1..]

fortunati' :: [Int] -> [a] -> [a]
fortunati' (g:gs) xs
    | null gs       = xs
    | otherwise     = fortunati' gs (togliOgni g xs)
