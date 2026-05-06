{-
insertion sort
merge sort
sumdown n = \sum^n_i=0 i
mdc(a,b) = {
    se a = b -> este é o mdc
    se a != b -> subtraia o menor do maior e repita o processo
}
dec2int: converte lista de dígitos para número, dc2int [2,3,5,6] = 2357 (use foldl ou foldr)
curry: converte uma fn que recebe um argumento tupla (x,y) em uma fn que recebe os argumentos separadamente
-}

insercaoSort :: Ord a => [a] -> [a]
insercaoSort [] = []
insercaoSort [x] = [x]
insercaoSort (x:xs) = insercao x (insercaoSort xs)

insercao :: Ord a => a -> [a] -> [a]
insercao x [] = [x]
insercao x (y:ys) = if x <= y then x : y : ys else y : insercao x ys

halves :: [a] -> ([a], [a])
halves xs = (ll, rl)
    where 
        ll = take (length xs `div` 2) xs
        rl = drop (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge lsorted rsorted
    where
        (ll, rl) = halves xs
        lsorted = msort ll
        rsorted = msort rl

merge :: Ord a => [a] -> [a] -> [a]
merge ll [] = ll
merge [] rl = rl
merge (x:xs) (y:ys) =
    if x <= y then
        x : merge xs (y:ys)
    else
        y : merge (x:xs) ys


sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown(x-1)

mdc :: Int -> Int -> Int
mdc a b =
    if a == b then
        a
    else
        if a > b then
            mdc (a-b) b
        else
            mdc a (b-a)

dec2int :: [Int] -> Int
dec2int xs = foldl (\n x -> n * 10 + x) 0 xs

curry' :: ((x,y) -> a) -> (x -> y -> a)
curry' f x y = f (x, y)

-------

uncurry' :: (x -> y -> a) -> ((x, y) -> a)
uncurry' f (x,y) = f x y

uncurry'' :: (x -> y -> a) -> ((x, y) -> a)
uncurry'' f = \(x,y) -> f x y

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- o operador (l !! n) pega o elemento n na lista l
fib' :: Int -> Integer
fib' n = fibs !! n

dig2int :: [Int] -> Int
dig2int xs = foldl (\acc x -> x + 10 * acc) 0 xs

bin2int :: [Int] -> Int
bin2int xs = foldr (\x acc -> x + 2 * acc) 0 xs

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
