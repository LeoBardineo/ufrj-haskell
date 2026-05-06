produto :: [Integer] -> Integer
produto [] = 1
produto (x:xs) = x * produto xs

q_sort :: [Integer] -> [Integer]
q_sort [] = []
q_sort (x:xs) = q_sort(menores) ++ [x] ++ q_sort(maiores)
    where
        menores = [y | y <- xs, y < x]
        maiores = [z | z <- xs, z >= x]

q_sort' :: [Integer] -> [Integer]
q_sort' [] = []
q_sort' (x:xs) = q_sort'(maiores) ++ [x] ++ q_sort'(menores)
    where
        menores = [y | y <- xs, y < x]
        maiores = [z | z <- xs, z >= x]
