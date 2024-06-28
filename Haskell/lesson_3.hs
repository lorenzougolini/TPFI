import System.Win32 (xBUTTON1, SECURITY_ATTRIBUTES (nLength))
mcd x y
    | x == y    = x
    | x < y     = mcd(y-x) x
    | otherwise = mcd(x-y) y

type Tile = (Int, Int)
type Move = (Int, Int)

move :: Tile -> Move -> Tile
move (x, y)(dx, dy) = (x+dx, y+dy)

move' :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
move' (x, y)(dx, dy) = (x+dx, y+dy)

type Pair a b = (a, b)
type PairH a = (a, a)
maxP (x, y) = if x < y then y else x

fstP :: PairH a -> a
fstP (x, y) = x

fstQ :: Pair a b -> a
fstQ (x, y) = x

data SetteNani = Eolo | Pisolo | Brontolo | Gongolo | Mammolo | Dotto | Cucciolo
height :: Num a => SetteNani -> a
height Cucciolo = 99
height _ = 124

data Days = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Eq, Ord, Show, Enum, Read)

data Product a b = P a b
myFst (P x y) = x
mySnd (P x y) = y

data Either a b = Lft a | Rgt b

safediv :: Integral a => a -> a -> Maybe a
safediv n 0 = Nothing
safediv n m = Just (n `div` m)

data Tree a = T a [Tree a]
    deriving Show

