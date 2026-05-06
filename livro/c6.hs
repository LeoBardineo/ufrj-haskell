-- sumdown :: Int -> Int

halve :: Ord a => [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (leftlist, rightlist)
    where
        leftlist    = take q xs
        rightlist   = drop q xs
        (q, r)      = length xs `divMod` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ll) (msort rl)
    where
        (ll, rl) = halve xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] rl = rl
merge ll [] = ll
merge (x:xs) (y:ys) =
    if x <= y then
        x : merge xs (y:ys)
    else
        y : merge (x:xs) ys

and' :: [Bool] -> Bool
and' [] = True
and' (p:ps) = p && and' ps
