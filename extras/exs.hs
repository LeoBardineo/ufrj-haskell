alternate :: Num a => [a] -> a
alternate [] = 0
alternate xs = foldr (\acc x -> acc + (-1 * x)) 0 xs

main :: IO ()
main = do
  print $ alternate [1, 2, 3, 4, 5]
