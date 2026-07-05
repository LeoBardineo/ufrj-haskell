-- recriando fold left
{- loop :: [a] -> [a] -> [a]
loop acc [] = acc
loop acc (x:xs) = loop (x:acc) xs -}
-- https://wiki.haskell.org/Foldr_Foldl_Foldl%27

rev :: [a] -> [a]
rev xs = loop [] xs
    where        
        loop acc [] = acc
        loop acc (x:xs) = loop (x:acc) xs

suml :: Num a => [a] -> a
suml xs = loop 0 xs
    where
        loop acc [] = acc
        loop acc (x:xs) = loop (acc + x) xs

dobral :: (b -> a -> b) -> b -> [a] -> b
dobral updateAcc initAcc xs = loop initAcc xs
    where
        loop acc [] = acc
        loop acc (x:xs) = loop (updateAcc acc x) xs

reverso :: [a] -> [a]
reverso = dobral updateAcc initAcc
    where
        initAcc = []
        updateAcc a x = x : a

suml' :: [Integer] -> Integer
suml' = dobral updateAcc initAcc
    where
        initAcc = 0
        updateAcc = (+)

fib :: Integer -> Integer
fib n = loop (0,1) n
    where
        loop (b0, b1) 0 = b1
        loop (b0, b1) n = loop (b1, b0+b1) (n-1)

-- [..] !! n (pegar n elemento)

meuapply :: (a -> b) -> a -> b
meuapply f x = f x

meupipe :: a -> (a -> b) -> b
meupipe x f = f x 

domau :: a -> b
domau x = domau x

todososnumeros :: [Integer]
todososnumeros = 0 : map (+1) todososnumeros

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
