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

-- curry: converte uma fn que recebe um argumento tupla (x,y)
-- em uma fn que recebe os argumentos separadamente
curry' :: ((x,y) -> a) -> (a -> a -> a)
curry' ((x,y) -> a) = a(a(x) a(y))
