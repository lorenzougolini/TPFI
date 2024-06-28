class Functor t where
    fmap :: (a -> b) -> t a -> t b

-- lista instanza di Functor
instance Functor [] where
    fmap = map

-- Maybe instanza di Fucntor
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

-- BinTree instanza di Functor
instance Functor BinTree where
    fmap f (F x) = F (f x)
    fmap f (R r lft rgt) = R (f r) (fmap f lft) (fmap f rgt)

inc :: Functor t => t Int -> t Int
inc = fmap (+1)


transpose :: [[a]] -> [[a]]
transpose []        = repeat []
transpose (xs:xss)  = repeat (:) `zapp` xs `zapp` (transpose xss)

repeat :: a -> [a]
repeat x = x : repeat x

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs) (x:xs)  = f x : zapp fs xs
zapp _ _            = []   

data Exp v = Var v | Val Int | Add Exp Exp
eval :: Exp v -> Env v -> Int
eval (Var x) env = fetch x env
eval (Val i) env = i
eval (Add e1 e2) env = eval e1 env + eval e2 env

-- L’ambiente di valutazione viene ‘distribuito’ da S, e dimenticato 
-- da K (quando non serve)
eval :: Exp v -> Env v -> Int
eval (Var x) = fetch x -- trova il valore di x
eval (Val i) = K i 
eval (Add e1 e2) = K (+) `S` eval e1 `S` eval e2

-- dove K e S sono i combinatori noti:
K x y = x
S x y z = x z (y z)

class Functor t => Applicative t where
    pure :: a -> t a
    (<*>) :: t (a -> b) -> t a -> t b

instance Applicative Maybe where
    pure = Just
    Nothing <*> _   = Nothing
    (Just g)<*> mx  = fmap g mx

instance Applicative [] where
    -- pure :: a -> [a]
    pure x = [x]

    -- <*> :: [a -> b] -> [a] -> [b]
    gs  <*> xs = [g x | g <- gs, x <- xs]

newType ZipList a = Z a
instance Applicative ZipList where
    pure x = Z (repeat x)
    Z fs <*> Z xs = Z (zipWith (\f x -> f x) fs xs)
