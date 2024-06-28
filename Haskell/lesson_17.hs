import Control.Monad.ST
import Data.ByteString.Char8 (readInt)

data Term = Const Int | Div Term Term
    deriving Show

newtype Out a = O (a, String) 
    deriving Show

instance Functor Out where
    fmap f (O(x, s)) = O(f x, s)

instance Applicative Out where
    pure x = O(x, "")
    (O(f, x)) <*> (O(a,y)) = O(f a, x++y)

instance Monad Out where
    (O(a,x)) >>= f = let (O(b,y)) = f a in O(b,x++y)

app (O(a, s)) = a

line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ ['\n']
logs (O(v,s)) t = O(v, "eval" ++ show t ++ " <= " ++ show v ++ ['\n'] ++ s)
evalOut (Const a) = O(a, line(Const a) a)
evalOut (Div t u) = 
    evalOut t >>= \a -> 
    evalOut u >>= \b -> 
        logs (pure (a `div` b)) (Div t u)


-- data BinTree a = F a | R (BinTree a) (BinTree a)
-- -- costruire albero da lista
-- newtype ST a = S (Main.State -> (a, Main.State))
-- type State = [Int]

-- buildM xs = fst (app (buildM' (length xs)) xs)
-- buildM' 1 = S (\s -> (F (head s), tail s))
-- buildM' n = do
--         u <- buildM' m
--         v <- buildM' (n-m)
--         return (R u v)
--     where m = n `div` 2

------------------- IO ---------------------
-- type IO a = World -> (a, World)
-- getChar :: IO Char
-- putChar :: Char -> IO ()
-- return :: a -> IO a
-- putStrLn :: String -> IO()
-- putStrLn xs = foldr (>>) done (map putChar xs) >> putChar '\n'

-- getLine :: IO String
-- getLine = getChar >>= f where
--     f x = if x == '\n' then return [] else
--         Main.getLine >>= g where 
--             g xs = return (x:xs)
-- oppure
-- getLine = getChar >>= \x -> f where
--     if x == '\n' then return [] 
--     else getLine >>= \xs -> return (x:xs)

-- con do notation
-- getLine = do
--     x <- getChar 
--     if x == '\n' then return [] else
--         do xs <- getLine
--             return (x:xs)

-- putStr [] = return ()
-- putStr (x:xs) = do
--     putChar x
--     putStr xs

-- putStrLn xs = do
--     putStr xs
--     putChar '\n'