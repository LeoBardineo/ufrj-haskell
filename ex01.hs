-- implementar take, length, elem, init, reverse, sum

-- take
primeiros :: Int -> [a] -> [a]
primeiros _ [] = []
primeiros 0 _ = []
primeiros i (x:xs) = x : primeiros (i-1) xs

-- length
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + (tamanho xs)

-- elem
elemento :: Eq t => t -> [t] -> Bool
elemento a [] = False
elemento a (x:xs) 
    | a == x = True
    | otherwise = elemento a xs

-- init
pop :: [t] -> [t]
pop [x] = []
pop (x:xs) = x : pop xs

-- reverse
reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

--sum
soma :: Num t => [t] -> t
soma [x] = x
soma (x:xs) = x + soma xs
