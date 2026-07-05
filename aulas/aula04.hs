-- implementar take, length, elem, init, reverse, sum

take_ :: Int -> [a] -> [a]
take_ _ [] = []
take_ 0 _ = []
take_ i (x:xs) = x : take_ (i-1) xs

length_ :: [a] -> Int
length_ [] = 0
length_ (x:xs) = 1 + (length_ xs)
