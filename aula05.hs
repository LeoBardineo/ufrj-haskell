-- com list comprehension
-- doubleList :: [Integer] -> [Integer]
-- doubleList xs = [2 * x | x <- xs]

doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (x:xs) = (2*x) : doubleList xs

notList :: [Bool] -> [Bool]
notList [] = []
notList (x:xs) = (not x) : notList xs

-- ^ operação aplicada a uma lista
-- map
mapa :: (a -> b) -> [a] -> [b]
mapa f [] = []
mapa f (x:xs) = (f x) : mapa f xs

isEven :: Integer -> Bool
isEven n = (n `mod` 2) == 0

isEvenAsList :: Integer -> [Integer]
isEvenAsList x | isEven x = [x]
isEvenAsList x | otherwise = []

-- evens xs = [x | x <- xs, isEven x]
evens :: [Integer] -> [Integer]
evens [] = []
evens (x:xs) = isEvenAsList x ++ evens xs

evens' :: [Integer] -> [Integer]
evens' [] = []
evens' (x:xs) | isEven x = x : evens xs
evens' (x:xs) | otherwise = evens xs

import Data.Char
onlyLowers :: [Char] -> [Char]
onlyLowers [] = []
onlyLowers (c:cs) | isLower c = c : onlyLowers cs
onlyLowers (c:cs) | otherwise = onlyLowers cs

-- ^ retorna elementos que obedecem a uma condição (predicado)
-- filter
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar p [] = []
filtrar p (x:xs) | p x          = x : filtrar p xs
filtrar p (x:xs) | otherwise    = filtrar p xs

-- redução eta
onlyLowers' :: String -> String
onlyLowers' = filter isLower

evens'' ::[Integer] -> [Integer]
evens'' = filter isEven

-- :t flip

-- lambda \
evens''' = filter (\x -> x `mod` 2 == 0)

impares :: [Integer] -> [Integer]
impares = filtrar (\x -> not (isEven x))

compoe :: (b -> c) -> (a -> b) -> (a -> c)
compoe f g = \x -> f (g x)

compoe' :: (b -> c) -> (a -> b) -> (a -> c)
compoe' f g x = f (g x)

-- filter (compoe not isEven) [1..20]

-- composição de funções .
impares'' = filtrar (not . isEven)


