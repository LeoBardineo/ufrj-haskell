import GHC.Float

oi = 10

avg :: [Float] -> Float
avg xs = sum xs / int2Float (length xs)

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

soma3 :: Integer -> Integer -> Integer -> Integer
soma3 a b c = a + b + c

-- curry
-- partial application

soma2 :: Integer -> Integer -> Integer
soma2 a b = a + b

-- foo :: String -> (Integer -> Integer)
-- foo _ = fac

somapar :: (Integer, Integer) -> Integer
somapar (a,b) = a + b

coloca (x,y) z = (x,y,z)

append :: [a] -> a -> [a]
append xs x = xs ++ [x]

-- listas, cons operator (:), list concat (++)
-- []
-- [1, 2, 3, 4, 5]
-- 30 : [40,50]
-- let xs = [40, 50]
-- let ys = 30 : x
-- (10: (20 : (30 : []))) é o mesmo que [10, 20, 30], [] é como um null
-- [10,20,30] ++ [40,50,60]

-- casamento de padrões
comprimento :: [a] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs

concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = x : concatena xs ys

-- ys = [30]
-- concat (10:20:[]) (30:[])
-- 10 : (concat (20:[]) (30:[]))
-- 10 : 20 : (concat [] (30:[]))
-- 10 : 20 : 30 : []

type Point = (Float, Float)

dist :: Point -> Point -> Float
-- dist (x1,y1) (x2, y2) = sqrt ((x1-x2) * (x1-x2) + (y1-y2) * (y1-y2))
dist (x1,y1) (x2, y2) =
    let dx = x1-x2 in
    let dy = y1-y2 in
    sqrt (dx * dx + dy * dy)

dist (x1,y1) (x2, y2) =
    let
        dx = x1-x2
        dy = y1-y2
    in
    sqrt (dx * dx + dy * dy)


dist (x1,y1) (x2, y2) =
    sqrt (dx * dx + dy * dy)
    where
        dx = x1-x2
        dy = y1-y2

-- https://philipnilsson.github.io/Badness10k/posts/2017-05-07-escaping-hell-with-monads.html