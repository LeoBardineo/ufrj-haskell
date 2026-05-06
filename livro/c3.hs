bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
-- nums = [[1..x] | x <- [1..4]]
nums = [[1..x] | x <- [1..]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f a = f a
