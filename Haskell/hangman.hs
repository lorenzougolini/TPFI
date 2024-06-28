import System.IO (hSetEcho, stdin)

hangman :: IO()
hangman = do
    putStrLn "Think a word: "
    word <- getSecretLine
    putStrLn "Try to guess it: "
    play word

getSecretLine :: IO [Char]
getSecretLine = do
    x <- getSecretChar
    if x == '\n' then do
        putChar x
        return []
    else do
        putChar '-'
        xs <- getSecretLine
        return (x:xs)

getSecretChar :: IO Char
getSecretChar = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

play :: [Char] -> IO ()
play word = do
    putStr "?"
    guess <- getLine
    if guess == word then 
        putStrLn "You got it!"
    else do 
        putStrLn (match word guess)
        play word

match :: Foldable t => [Char] -> t Char -> [Char]
match xs ys = [if elem x ys then x else '-' | x <- xs]