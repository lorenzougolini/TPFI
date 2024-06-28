-- Step 1: Definire il tipo NatBin
data NatBin = Zero | One | Bit NatBin
    deriving (Eq, Show)

-- Funzione di aiuto per convertire un NatBin in un numero intero
natBinToInt :: NatBin -> Int
natBinToInt Zero = 0
natBinToInt One = 1
natBinToInt (Bit b) = 2 * natBinToInt b

-- Funzione di aiuto per convertire un numero intero in un NatBin
intToNatBin :: Int -> NatBin
intToNatBin 0 = Zero
intToNatBin 1 = One
intToNatBin n = Bit (intToNatBin (n `div` 2))

-- Step 2: Definire un valutatore per le espressioni aritmetiche
data Expr = Const NatBin | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Mod Expr Expr

eval :: Expr -> Maybe NatBin
eval (Const n) = Just n
eval (Add e1 e2) = do
    n1 <- eval e1
    n2 <- eval e2
    return $ intToNatBin ((natBinToInt n1) + (natBinToInt n2))
eval (Sub e1 e2) = do
    n1 <- eval e1
    n2 <- eval e2
    let result = (natBinToInt n1) - (natBinToInt n2)
    if result < 0 then Nothing else return $ intToNatBin result
eval (Mul e1 e2) = do
    n1 <- eval e1
    n2 <- eval e2
    return $ intToNatBin ((natBinToInt n1) * (natBinToInt n2))
eval (Div e1 e2) = do
    n1 <- eval e1
    n2 <- eval e2
    if natBinToInt n2 == 0 then Nothing else return $ intToNatBin ((natBinToInt n1) `div` (natBinToInt n2))
eval (Mod e1 e2) = do
    n1 <- eval e1
    n2 <- eval e2
    if natBinToInt n2 == 0 then Nothing else return $ intToNatBin ((natBinToInt n1) `mod` (natBinToInt n2))

-- Step 4: Rendere NatBin un'istanza delle classi Eq, Ord, Num, Show

instance Ord NatBin where
    compare n1 n2 = compare (natBinToInt n1) (natBinToInt n2)

instance Num NatBin where
    (+) n1 n2 = intToNatBin $ (natBinToInt n1) + (natBinToInt n2)
    (-) n1 n2 = let result = (natBinToInt n1) - (natBinToInt n2) in if result < 0 then Zero else intToNatBin result
    (*) n1 n2 = intToNatBin $ (natBinToInt n1) * (natBinToInt n2)
    abs n = n
    signum n = if n == Zero then Zero else One
    fromInteger n = intToNatBin (fromInteger n)

-- Per completare l'implementazione, testiamo alcune operazioni
main :: IO ()
main = do
    let n1 = intToNatBin 5
    let n2 = intToNatBin 3
    print $ eval (Add (Const n1) (Const n2)) -- Dovrebbe stampare Just (Bit (Bit One)) che rappresenta 8 in binario
    print $ eval (Div (Const n1) (Const n2)) -- Dovrebbe stampare Just (Bit One) che rappresenta 1 in binario
    print $ eval (Div (Const n1) (Const Zero)) -- Dovrebbe stampare Nothing (divisione per zero)
