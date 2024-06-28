import Data.List

sumsqreven :: Integral a => [a] -> a
sumsqreven xs = sum (map (^2) (filter even xs))

votes = ["Rossi", "Bianchi", "Verdi", "Bianchi", "Bianchi", "Rossi"]
rmvDups (x:xs) = x : filter (/=x) (rmvDups xs)
rmvDups []   = []

results :: Ord a => [a] -> [(Int, a)]
results vs = sort [(count v vs, v) | v <- rmvDups vs] where
    count x = length . filter (==x)

qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort smaller ++ [x] ++ qSort larger where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]


votes' = [["Rossi", "Verdi"], ["Bianchi"], ["Verdi", "Rossi", "Bianchi"], ["Bianchi", "Verdi", "Rossi"], ["Verdi"]]

rank :: Ord a =>  [[a]] -> [a]
rank = map snd . results . map head

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/=x))
rmvEmpty = filter (/=[])
looserRound = snd . head . results . map head

voteRound :: (t, [[String]]) -> (String, [[String]])
voteRound = \(x, xs) -> let e = looserRound xs
                            in (e, rmvEmpty (elim e xs))

noOne = null . snd
winnerB'' = fst . until noOne voteRound
winnerS xs = winnerB'' (undefined, xs)

-- COMMON WORDS --

type Text = [Char]
type Word = [Char]
commmonWords :: Int -> Text -> String
commmonWords n =
    showOutput                      -- [(Int, Word)] -> String
        . take n                    -- [(Int, Word)] -> [(Int, Word)]
            . mySort                -- [(Int, Word)] -> [(Int, Word)]
                . countFreqs'        -- [Word] -> [(Int, Word)]
                    . findWords     -- Text -> [Word]

findWords :: Text -> [Main.Word]
findWords = map toLowerAlpha . words

ts = zip ['a'..'z'] ['A'..'Z']
toLowerAlpha :: Main.Word -> Main.Word
toLowerAlpha (c:cs) = 
    if null ws then rs else fst (head ws):rs where
        ws = filter (\(l,u) -> l == c || u == c) ts
        rs = toLowerAlpha cs
toLowerAlpha [] = []

countFreqs :: [Main.Word] -> [(Int, Main.Word)]
countFreqs = results

mySort :: [(Int, Main.Word)] -> [(Int, Main.Word)]
mySort = reverse . qSort

showOutput :: [(Int, Main.Word)] -> String
showOutput = concat . map showFreqs
showFreqs (n, w) = w ++ ": " ++ show n ++ ['\n'] 

countSeqs [] = []
countSeqs(x:xs) = (1+length fs, x):countSeqs ls where
    (fs, ls) = span (==x) xs

countFreqs' :: [Main.Word] -> [(Int, Main.Word)]
countFreqs' = countSeqs . qSort

commmonWords' :: Int -> Text -> String
commmonWords' n =
    showOutput                      -- [(Int, Word)] -> String
        . take n                    -- [(Int, Word)] -> [(Int, Word)]
            . mySort                -- [(Int, Word)] -> [(Int, Word)]
                . countFreqs'        -- [Word] -> [(Int, Word)]
                    . findWords     -- Text -> [Word]

powerset :: [a] -> [[a]]
powerset (x:xs) = map (x:) ts ++ ts where
    ts = powerset xs
powerset [] = [[]]

combinations :: [a] -> Int -> [[a]]
combinations xs k = cmbs xs (length xs) k
cmbs :: (Eq t, Num t) => [a] -> t -> t -> [[a]]
cmbs xs@(x:txs) n k 
    | n == k    = [xs]
    | k == 0    = [[]]
    | otherwise = map (x:) (cmbs txs (n-1) (k-1)) ++ cmbs txs (n-1) k