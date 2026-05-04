todos :: (a -> Bool) -> [Bool] -> Bool
todos fn xs = length (filter (== False) (map fn xs)) == 0
