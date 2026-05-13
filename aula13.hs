{-
type IO a = Mundo -> (a, Mundo)
putchar :: Char -> IO ()
getchar :: IO Char

echo 2 :: IO ()
echo 2 =
    getchar >>= (\c -> 
        putchar c >>= (\_ ->
            putchar c ))

echo2 :: IO ()
echo2 = do
    c <- getchar
    putchar c
    putchar c

return :: a -> IO a
>>= :: IO a -> (a -> IO b) -> IO b

data World
runIO :: IO a -> Mundo -> (a, Mundo)
m >>= f = (\w -> 
    let (x, w') = runIO m w in
    let (y, w'')= runIO (f x) w' in
    (y, w'')
)
-}

-- >>=
fish :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
--fish f g = (\x -> (f x) >>= g)
fish f g = (\x -> f x >>= (\b -> g b))

-- fazer exercícios capitulo 10

main :: IO ()
main = do
    x <- return "Hugo"
    y <- return 17
    return "Leonardo"
    print x
    print y
