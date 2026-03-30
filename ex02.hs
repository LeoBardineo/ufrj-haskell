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
media l = sum l / fromIntegral(length l)
