-- fazer exercícios do livro, 4 e 5

length' :: [a] -> int
length' [] = 0                  -- (1)
length' (x:xs) = 1 + length' xs -- (2)
-- length' = foldr f z
--     where
--         f x y = 1 + y -- (\x y -> 1 + y)
--         z = 0
-- length const(+1) 0

replicate' :: Int -> a -> [a]
replicate' 0 x = []                     -- (1)
replicate' n x = x : replicate' (n-1) x -- (2)

{-
vamos provar que ∀n∀x.(length (replicate n x) = n)
obs.: o prop P(n) = ∀x.len(rep n x) = n

vamos provar por indução em n
caso 0: queremos provar que ∀x(length(replicate 0 x) = 0)
prova:
length(rep 0 x)
= rep.1
length []
= len.1
0

caso n+1: queremos provar que ∀x(len(rep(n+1) x) = n+1)
assumindo que ∀x (len(rep n x) = n)
prova:
len(rep(n+1) x)
= rep.1
len(x : rep n x)
= len.2
1 + len $ rep n x
= H.I
1 + n
a soma é comutativa, Q.E.D

-}

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

{-
pesquisar indução point free
xs lista finita
provar ∀xs $ rev (rev xs) = xs
indução na lista
P([])
∀x∀xs(P(xs) => P(x:xs))

caso []:
    rev(rev([]))
    rev([])
    []
caso (x:xs):
    queremos provar rev(rev(x:xs)) = (x:xs)
    assumindo que rev(rev xs) = xs
    rev(rev(x:xs))
    = rev.1
    rev((rev xs) ++ [x])
    = {rev(xs ++ ys) = rev ys + rev xs}
    rev[x] ++ rev(rev xs)
    = {H.I.}
    rev(x:[]) ++ xs
    = rev.1
    rev [] ++ [x] ++ xs
    = {def (++)}
    x : xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

caso []:
    queremos provar rev([] ++ ys) = rev ys ++ rev []
    rev([] ++ ys)
    = rev(ys)

    rev ys ++ rev []
    rev ys ++ []
    falta provar (as ++ [] = as)

caso (x:xs):
    queremos provar rev((x:xs) ++ ys) = rev ys ++ rev (x:xs)
    assumindo rev(xs ++ ys) = rev ys ++ rev xs
    esquerda:
    rev ((x:xs) ++ ys)
    = { (++).2 }
    rev (x : (xs ++ ys))
    = { rev.1 }
    rev (xs ++ ys) ++ [x]
    = { H.I. }
    (rev ys ++ rev xs) ++ [x]
    = { falta provar ++ assoc }
    rev ys ++ (rev xs ++ rev [x])

    direita:
    rev ys ++ rev (x:xs)
    rev ys ++ (rev xs ++ [x])

provar ++ assoc
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)
esquerda
x : (xs'++ys) ++ zs
x : (xs'++ys++zs)

direita
xs ++ (y:(ys'++zs))

provar ∀n∀xs $ len xs = n => rev(rev(x)) = xs

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x:xs) = p x && all p xs

prove que ∀n∀x $ all (==x) (replicate n x)
indução em n
H.I -> all (==x) (replicate n x) => True
caso 0:
    all (==x) (replicate 0 x)
    = { rep.1 }
    all (==x) ([])
    = { all.1 }
    True
caso n+1:
    all (==x) (replicate (n+1) x)
    = { rep.2 }
    all (==x) (x : replicate n x)
    = { all.2 }
    (x==x) && all (==x) replicate n x
    = { H.I }
    (x==x) && True
    True && True
    True

-}
