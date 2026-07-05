-- 9.3

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = (x `mod` y) == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o e1 e2) = brak e1 ++ so ++ brak e2
        where
            brak (Val n) = show n
            brak e = "("++ show e ++")"
            so = show o

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

evalm :: Expr -> Maybe Int
evalm (Val n) = Just n
evalm (App o l r) = 
    case evalm l of
        Nothing -> Nothing
        Just x ->
            case evalm r of
                Nothing -> Nothing
                Just y ->
                    if valid o x y then
                        Just (apply o x y)
                    else
                        Nothing

{-
eval $ App Sub (Val 17) (Val 17)
eval $ App Sub (Val 17) (Val 16)
-}
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) =
    [apply o x y | x <- eval l,
        y <- eval r,
        valid o x y]

-- 9.4 combinatorial
-- https://oeis.org
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
    where
        yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = 
    (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = 
    concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- is_solution e765 [1,3,7,10,25,50] 765
is_solution :: Expr -> [Int] -> Int -> Bool
is_solution e ns target = 
    eval e == [target] && elem (values e) (choices ns)

e765 :: Expr
e765 = App Mul  (App Add (Val 1) (Val 50))
                (App Sub (Val 25) (Val 10))

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) =
    ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs xs =
    [ e |
        (lis, ris) <- split xs,
        le <- exprs lis,
        re <- exprs ris,
        e <- combine le re
    ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions available target =
    [ e |
        xs <- choices available,
        e <- exprs xs,
        eval e == [target]
    ]

-- ghc -O2 aula10.hs
import Control.Monad

main :: IO ()
main = mapM_ print (solutions [1, 3, 7, 10, 25, 50] 765)
