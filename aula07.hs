-- recriando fold left
{- loop :: [a] -> [a] -> [a]
loop acc [] = acc
loop acc (x:xs) = loop (x:acc) xs -}

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
