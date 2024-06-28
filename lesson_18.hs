import Control.Monad.ST (runST, ST)
import Data.Array.Base (newListArray, getElems, readArray, writeArray)
import Data.Array.ST (STArray)
qSort xs = runST $ do
    xa <- newListArray(0, n-1) xs
    qsortST xa (0, n)
    getElems xa
    where n = length xs

qsortST :: Ord a => STArray s Int a -> (Int, Int) -> ST s ()
qsortST xa (a, b)
    | a == b = return ()
    | otherwise = do
        m <- partition xa (a, b)
        qsortST xa (a, m)
        qsortST xa (m+1, b)

partition xa (a, b) = do
    x <- readArray xa a
    let loop (j, k) = if j == k then do
            swap xa a (k-1)
            return (k-1)
        else do
            y <- readArray xa j
            if y < x then loop (j+1, k)
            else do
                swap xa j (k-1)
                loop (j, k-1)
        in loop (a+1, b)

swap xa i j = do 
    v <- readArray xa i
    w <- readArray xa j
    writeArray xa i w
    writeArray xa j v

