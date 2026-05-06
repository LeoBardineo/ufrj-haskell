-- sem variáveis com letra maiúscula, reservada
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

ultimo :: [Integer] -> Integer
ultimo xs = head (reverse xs)

inicio :: [Integer] -> [Integer]
inicio [x] = []
inicio (x:xs) = x : inicio xs

inicio' :: [Integer] -> [Integer]
inicio' xs = take (length xs - 1) xs
