{-
aula05
removeNull  ->  Recebe uma lista de listas
                retorna a lista sem elementos que são lista vazia
media       ->  Recebe uma lista de Float
                retorna a média (crasha se a lista for vazia)
maxAvg      ->  Recebe uma lista de çostas de Float
                e retorna a maior média (ignorar listas vazias)
                variação: retorna a lista de maior média
capitalize  ->  converte a string para maiúsculo
-}

removeNull :: [[a]] -> [[a]]
removeNull [] = []
removeNull xs = filter (not . null) xs

media :: [Float] -> Float
media xs = sum xs / fromIntegral(length xs)

maxAvg :: [[Float]] -> Float
maxAvg [x] = media x
maxAvg (x:xs) | (media(x) >= maxAvg(xs)) = media(x)
maxAvg (x:xs) | otherwise = maxAvg(xs)

-- maxAvgList :: [[Float]] -> [Float]
-- maxAvgList [x] = x
-- maxAvgList (x:xs) | (media(x) >= maxAvgList(xs)) = x
-- maxAvgList (x:xs) | otherwise = maxAvgList(xs)
