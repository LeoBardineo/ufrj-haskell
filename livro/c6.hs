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
msort xs = merge ll rl
    where
        (ll, rl) = halve xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] rl = rl
merge ll [] = ll
merge [x] [y] = if x <= y then [x,y] else [y,x]
merge xs ys = merge ll rl
    where 
        (llxs, rlxs) = halve xs
        (llys, rlys) = halve ys
        ll = merge llxs rlxs
        rl = merge llys rlys
