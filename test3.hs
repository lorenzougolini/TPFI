-- import Control.Monad.State

-- data BinTree a = Empty | Node a (BinTree a) (BinTree a) 
--     deriving (Show)

-- -- Function to calculate the sum of all nodes in a subtree
-- sumFromU :: Num a => BinTree a -> a
-- sumFromU Empty = 0
-- sumFromU (Node v l r) = v + sumFromU l + sumFromU r

-- -- State to maintain the current sumToU and collect balanced nodes
-- type BalancedState a = State (a, [a])

-- -- Function to find balanced nodes
-- nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
-- nodiEquilibrati tree = snd $ execState (nodiEquilibratiAux tree) (0, [])

-- nodiEquilibratiAux :: (Num a, Eq a) => BinTree a -> BalancedState a ()
-- nodiEquilibratiAux Empty = BalancedState (\s -> ((), s))
-- nodiEquilibratiAux (Node u l r) = do
--     (sumToU, balancedNodes) <- get
--     let sommaSottoU = sumFromU (Node u l r)
--     when (sumToU == sommaSottoU) $
--         put (sumToU, u : balancedNodes)
--     modify (\(sumToU, balancedNodes) -> (sumToU + u, balancedNodes))
--     nodiEquilibratiAux l
--     nodiEquilibratiAux r

----------------------------- working ------------------------------
-- import Control.Monad.State

-- data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

-- -- Function to calculate the sum of all nodes in a subtree
-- sumFromU :: Num a => BinTree a -> a
-- sumFromU Empty = 0
-- sumFromU (Node v l r) = v + sumFromU l + sumFromU r

-- -- State to maintain the current sumToU and collect balanced nodes
-- type BalancedState a = State ([a], a)

-- -- Function to find balanced nodes
-- nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
-- nodiEquilibrati tree = fst $ execState (nodiEquilibratiAux tree 0) ([], 0)

-- nodiEquilibratiAux :: (Num a, Eq a) => BinTree a -> a -> BalancedState a a
-- nodiEquilibratiAux Empty _ = return 0
-- nodiEquilibratiAux (Node u l r) sumToU = do
--     let sommaSottoU = sumFromU (Node u l r)
--     when (sumToU == sommaSottoU) $ modify (\(balancedNodes, totalSum) -> (u : balancedNodes, totalSum))
--     sumL <- nodiEquilibratiAux l (sumToU + u)
--     sumR <- nodiEquilibratiAux r (sumToU + u)
--     return (u + sumL + sumR)
-------------------------------------------------------------------------


-- data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

-- -- Function to calculate the sum of all nodes in a subtree
-- sumFromU :: Num a => BinTree a -> a
-- sumFromU Empty = 0
-- sumFromU (Node v l r) = v + sumFromU l + sumFromU r

-- -- State to maintain the list of balanced nodes
-- type BalancedState a = State [a]

-- -- Function to find balanced nodes
-- nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
-- nodiEquilibrati tree = execState (nodiEquilibratiAux tree 0) []

-- nodiEquilibratiAux :: (Num a, Eq a) => BinTree a -> a -> BalancedState a a
-- nodiEquilibratiAux Empty sumToU = return 0
-- nodiEquilibratiAux (Node u l r) sumToU = do
--     sommaL <- nodiEquilibratiAux l (sumToU + u)
--     sommaR <- nodiEquilibratiAux r (sumToU + u)
--     let sommaSottoU = u + sommaL + sommaR
--     when (sumToU == sommaSottoU) $
--         modify (u :)
--     return sommaSottoU

-- -- Example usage
-- main :: IO ()
-- main = do
--     let tree1 = Node 1 (Node 3 (Node 4 Empty Empty) Empty) (Node 2 Empty Empty)
--     let tree2 = Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty)
--     print $ nodiEquilibrati tree1 -- Should return [4]
--     print $ nodiEquilibrati tree2 -- Should return [4]

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

-----------------------------------------------------------------------------

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
    if n < 0 then Nothing 
    else case checkBinWord (reverse (intToNatBinAux n)) of
            Nothing -> Nothing
            Just x -> Just x 
            where
                intToNatBinAux 0 = []
                intToNatBinAux n = let (q,r) = n `divMod` 2 in r : intToNatBinAux q

natBinToInt :: NatBin -> Maybe Int
natBinToInt n = if length n == 8 && head n == 1 then Nothing else Just (foldl (\acc x -> acc * 2 + x) 0 n)

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