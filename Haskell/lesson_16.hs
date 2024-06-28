data Term = Const Int | Div Term Term
    deriving Show

eval :: Term -> Int
eval (Const a) = a
eval (Div t u) = eval t `div` eval u

-- eval (Div (Const 1) (Const 0)) da errore nella divisione per 0

eval :: Term -> Maybe Int
eval (Const a) = Just a
eval (Div t u) = case eval t of
    Nothing -> Nothing
    Just a -> case eval u of
        Nothing -> Nothing
        Just b -> if b == 0 then Nothing else Just (a `div` b)

-- produciamo i log della valutazione
type M a = (Output, a)
type Output = String

-- func line per printare
line :: (Show a1, Show a2) => a1 -> a2 -> [Char]
line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ ['\n']

eval :: Term -> M Int
eval (Const a) = (line (Const a) a, a)
eval (Div t u) = let (x, a) = eval t in
    let (y, b) = eval u in 
        (x ++ y ++ line (Div t u) (a `div` b), a `div` b)

-- contiamo il numero di divisioni eseguite
type M a = (a, State)
type State = Int

eval :: Term -> M Int
eval (Const a) x = (a, x)
eval (Div t u) x = let (a, y) = eval t x in
    let (b, z) = eval u y in
        (a `div` b, z+1)

class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a 
    -- return = pure

-- monade identità
instance Identity Monad where
    pure a = a
    -- (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    a >>= f = f a

instance [] Monad where
    xs >>= f = concat (map f xs)
    return x = [x]

eval :: Monad m => Term -> m Int
eval (Const a) = pure a
eval (Div t u) = eval t >>= \a -> eval u >>= \b -> pure (a `div` b)

------------------------------------------------------------------

newtype ST a = S (State -> (a, State))
type State = Int

app (S st) x = st x

tick (S st) = S (\s -> let (a, s') = st s in (a, s'+1))

instance Functor ST where
    fmap f st = S (\s -> let (x, s') = app st s
        in (f x, s')
        )

instance Applicative ST where
    pure x = S (\s -> (x, s))
    stf <*> stx = S (\s -> 
        let (f, s') = app stf s in
            let (x, s'') = app stx s' in
                (f x, s'')
                )

instance Monad ST where
    return = pure
    st >>= f = S (\s ->
        let (x, s') = app st s in
            app (f x) s'
            )

eval (Const a) = S (\s -> (a, s))
eval (Div t u) =
    eval t >>= \a ->
        eval u >>= \b -> 
            S (\s -> (a `div` b, s+1))

-- usando tick e pure
eval' (Const a) = S (\s -> (a, s))
eval' (Div t u) = 
    eval' t >>= \a -> 
        eval' u >>= \b -> 
            tick (pure a `div` b)

-- usando do notation e return
eval'' (Const a) = S (\s -> (a,s))
eval'' (Div t u) = do
    a <- eval'' t
    b <- eval'' u
    tick (return a `div` b)