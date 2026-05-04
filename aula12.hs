{-
(fibs \zw fibs')!!0

WHNF - Weak Head Normal Form
- Construtor C(x,y,z)
- lambdas (\lambda x . e)
- funções aplicadas parcialmente (const 3)

fibs    = 0 : fibs'
fibs'   = 1 : fibs''
fibs''  = fibs \zw fibs'
        = (0:fibs') \zw (1:fibs'')
        = 1:fibs'''
-}

fibs = 0 : fibs'
fibs' = 1 : zipWith (+) fibs fibs'

-- exercicios da aula passada
filterprimes :: [Integer] -> [Integer]
filterprimes [] = []
filterprimes [x] = [x]
filterprimes (x:xs) = x : filterprimes(filter (\y -> y `mod` x /= 0) xs)

primes :: [Integer]
primes = filterprimes [2..]

hamming :: [Integer]
hamming = [1,2,3,5] ++ multiplos2 ++ multiplos3 ++ multiplos5
    where
        multiplos2 = map (*2) (hamming)
        multiplos3 = map (*3) (hamming)
        multiplos5 = map (*5) (hamming)

mergesorted :: [Integer] -> [Integer] -> [Integer]
mergesorted [] ly = ly
mergesorted lx [] = lx
mergesorted [x] [y] = if x <= y then [x,y] else [y,x]
mergesorted lx ly = mergesorted $ (mergesorted leftlist(lx) rightlist(lx)) (mergesorted leftlist(ly) rightlist(ly))
-- mergesorted (x:xs) (y:ys) =
--     if x <= y then
--         x : y : mergesorted xs ys
--     else
--         y : x : mergesorted xs ys

{-
mergesort([1, 2, 4, 6, 7] [2, 3, 4, 8, 9])
mergesort([1, 2] [4, 6, 7]) mergesort([2, 3] [4, 8, 9])
-}

leftlist :: [Integer] -> [Integer]
leftlist xs = take q xs
    where
        (q,r) = length xs `divMod` 2

rightlist :: [Integer] -> [Integer]
rightlist xs = drop q xs
    where
        (q,r) = length xs `divMod` 2
