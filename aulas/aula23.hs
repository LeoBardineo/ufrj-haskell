reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

{-
queremos uma fn             rev' :: [a] -> [a] -> [a]
que cumpra a propriedade    rev' xs ys = (rev xs) ++ ys

vamos derivar uma implementação de rev' com essa propriedade
tentamos provar a propriedade e descobrimos qual passo falta
a prova será por indução em xs e terá dois casos:
caso []:
rev' [] ys  = (rev []) ++ ys
            = { def reverse 1 }
rev' [] ys  = [] ++ ys
            = { def (++) 1 }
rev' [] ys  = ys

caso (x:xs): ∀x, xs
se assumirmos   rev' xs ys = rev xs ++ ys
então vale      rev' (x:xs) ys = rev (x:xs) ++ ys
rev' (x:xs) ys  = rev (x:xs) ++ ys
                = { def rev 2 }
rev' (x:xs) ys  = (rev xs ++ [x]) ++ ys
                = { assoc ++ }  
rev' (x:xs) ys  = rev xs ++ ([x] ++ ys)
                = { def ++ }
rev' (x:xs) ys  = rev xs ++ (x:ys)
                = { H.I. }where
    --     f = \K -> \x -> (\ys -> K(x:ys))
    --     z = \ys -> ys
rev' (x:xs) ys  = rev' xs (x:ys)

se definirmos
    rev' [] ys = ys
    rev' (x:xs) ys = rev' xs (x:ys)
então é possível provar por indução em xs que
    rev' xs ys = rev xs ++ ys

exemplo do caso base em 1 col.
rev' [] ys
= { def rev' }
ys
= { def ++ }
[] ++ ys
= { def rev }
rev [] ++ ys
podemos partir do último até o primeiro também


-}

{-
⚠️ porção feita com foldl
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

rev' :: [a] -> ([a] -> [a]) -- o ([a] -> [a]) é o b do foldl
rev' = foldl f z
    where
        f K x   = \ys -> K(x:ys)
        z       = \ys -> ys
    -- where
    --     f = \K -> \x -> (\ys -> K(x:ys))
    --     z = \ys -> ys

rev' [] = \ys -> ys
rev' (x:xs) = \ys -> rev' xs (x:ys)

rev' [10,20,30]
\ys -> rev' [20,30] (10:ys)
\ys -> (\ys -> rev' [30] (20:ys)) (10:ys)
\ys -> (rev' [30] (20:10:ys))
\ys -> rev' [] (30:20:10:ys)
\ys -> 30:20:10:ys
-}

{-
rev' [] = \ys -> ys
rev' (x:xs) = \ys -> rev' xs (x:ys)

reverse xs = foldl f []
    where
        f = \ys x = x:ys
-}

{-
data Tree = Leaf Int | Node Tree Tree
flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

flatten’ t ns   = flatten t ++ ns

questão:
para provar ∀t P(t)
basta provar
1.  ∀n P(Leaf n)
2.  se vale P(l) ^ P(r)
    então vale P(Node l r)
exercício: P(t) = ∀nx (flatten' t ns = flatten t ++ ns)

caso (Leaf n):
flatten' t ns   = flatten t ++ ns
                = [n] ++ ns
                = (n : ns)

caso (Node l r):
flatten' t ns   = flatten t ++ ns
                = flatten l ++ flatten r ++ ns
                = flatten l ++ (flatten r ++ ns)
                = flatten' l (flatten r ++ ns)
                = flatten' l (flatten' r ns)

resolução:
1) ∀ns (flatten' (Leaf n) ns = flatten (Leaf n) ++ ns)

2) Assumindo
    H1: ∀ns (flatten' l ns = flatten l ++ ns)       a especificação vale para a árvore da esquerda
    H2: ∀ns (flatten' r ns = flatten r ++ ns)       a especificação vale para a árvore da direita
então flatten' (Node lr) ns = flatten (Node lr) ++ ns

-}