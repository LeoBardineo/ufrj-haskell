-- tipo de bool, char, lista de char, string, tupla, not, head, length
-- tail recursion, citou lazy evaluation
-- curry, avaliação parcial
-- erro de sintaxe, erro de tipo
-- polimorfismo
-- começou overload, typeclass

f :: Integer -> Integer
f 0 = 0
f n = 1 + f n

meu_ou :: Bool -> Bool -> Bool
meu_ou True True = True
meu_ou True False = True
meu_ou False True = True
meu_ou False False = False

meu_ou' :: Bool -> Bool -> Bool
meu_ou' True x = True
meu_ou' False x = meu_ou' x False

meu_ou'' :: Bool -> Bool -> Bool
meu_ou'' True x = True
meu_ou'' False x = x

meu_ou''' :: Bool -> Bool -> Bool
meu_ou''' False False = False
meu_ou''' _ _ = True

boom :: Integer -> Integer
boom 0 = 0
boom n = 1 + boom n

cabum :: Integer -> Integer
cabum n = cabum (n+1)

casee x =
    case x of
        True    -> 10-- head, length
    else if x == 0 then
        0
    else 
        1

soma (a,b) = a + b
soma' a b = a + b

--------------
-- polimorfismo
-- (a, b) -> b, tipos diferentes se encaixam ao tipo da expressão   
segundo (a,b) = b

id' x = x
