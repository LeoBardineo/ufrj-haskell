{-
(f o g)x = f(g(x))
------
map f [] = []
map f (x:xs) = f x : map f xs
------
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
------
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flaten r
------
flatten' t xs = flatten t ++ xs?
-}

{-
provar
map f o map g = map (f o g)
map f (xs ++ ys) = map f xs ++ map f ys

map f o map g   = map (f o g)
primeiro passo é colocar os argumentos
    axioma da extensionalidade
    ∀x (f x = g x) => f g
map f (map g xs) = map (f o g) xs
                => { fog 1 }
map (f o map g) xs
caso []:
    map (f o map g) []
    => { map 1 }
    []
caso (x:xs):
    map (f o map g) (x:xs)
    => { map 2 }
    (f o map g) x : map (f o map g) xs

r:
map f (map g xs) = map (f o g) xs
- map f (map g []) = map (f o g) []
- map f (map g (x:xs)) = map (f o g) (x:xs)
    assumindo map f (map g xs) = map (f o g) xs
caso []:
    map f (map g [])    = map (f o g) []    
    map f []            = []                { map 1 }
    []                  = []                { map 1 }
caso (x:xs):
    map f (map g (x:xs))            = map (f o g) (x:xs)
    map f (g x : map g xs)          = (f o g) x : map (f o g) xs    { map 2 }
    f (g (x)) : map f (map g xs)    = (f o g) x : map (f o g) xs    { map 2 }
    (f o g) x : map f (map g xs)    = (f o g) x : map (f o g) xs    { fog 1 }
    pela H.I. são iguais


map f (xs ++ ys) = map f xs ++ map f ys
indução em xs
- map f ([] ++ ys) = map f [] ++ map f ys
- map f ((x:xs) ++ ys) = map f (x:xs) ++ map f ys
    assumindo map f (xs ++ ys) = map f xs ++ map f ys
caso []:
    map f ([] ++ ys)    = map f [] ++ map f ys
    map f ys            = map f [] ++ map f ys  { [] 1 }
    map f ys            = [] ++ map f ys        { map 1 }
    map f ys            = map f ys              { [] 1 }
caso (x:xs):
    map f ((x:xs) ++ ys)    = map f (x:xs) ++ map f ys
    map f (x : (xs ++ ys))  = map f (x:xs) ++ map f ys      { ++ 2 }
    f x : map f (xs ++ ys)  = map f (x:xs) ++ map f ys      { map 2 }
    f x : map f (xs ++ ys)  = (f x : map f xs) ++ map f ys  { map 2 }
    f x : map f (xs ++ ys)  = f x : (map f xs ++ map f ys)  { ++ 2 }
    pela H.I. são iguais


flatten' t xs = flatten t ++ xs
- flatten' (Leaf n) xs = flatten (Leaf n) ++ xs
- flatten' (Node l r) xs = flatten (Node l r) ++ xs
caso Leaf n:
    flatten' (Leaf n) xs    = flatten (Leaf n) ++ xs    
                            = [n] ++ xs                  { flatten 1 }
                            = n : ([] ++ xs)             { ++ 2 }
                            = n : xs                     { ++ 1 }
caso Node l r:
    flatten' (Node l r) xs  = flatten (Node l r) ++ xs
                            = flatten l ++ flatten r ++ xs      { flatten 2 }
                            = flatten l ++ (flatten r ++ xs)
                            = flatten' l (flatten r ++ xs)      
                            = flatten' l (flatten' r xs)        
temos então que definir:
- flatten' (Leaf n) xs = n : xs
- flatten' (Node l) xs = flatten l ++ xs

-}
