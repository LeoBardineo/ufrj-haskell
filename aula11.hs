-- avaliação por fora, por dentro, sob demanda
-- REDEX
-- livro SPJ implementação de linguagens funcionais preguiçosas

square :: Integer -> Integer
square x = x * x

constante :: a -> b -> a
constante x y = x

meu_and False x = False
meu_and True x = x

meu_and' x False = False
meu_and' x True = x

loop :: a
loop = loop

loop' :: Integer -> Integer
loop' n = loop' $! (n+1)

meu_if :: Bool -> a -> a -> a
meu_if True t f = t
meu_if False t f = f

---------
-- estruturas infinitas, avaliação estrita

uns :: [Integer]
uns = 1 : uns

nats :: [Integer]
nats = go 0 
    where
        go:: Integer -> [Integer]
        go n = n : (go $! n + 1)

nats' :: [Integer]
nats' = 0 : map (+1) nats'

fibs, fibs' :: [Integer]
fibs    = 0 : fibs'
fibs'   = 1 : zipWith (+) fibs fibs'

{-
take 4 fibs
take 4 (0 : fibs')
0 : take 3 fibs'
0 : take 3 (1 : fibs ⊕ fibs')
0 : 1 : take 2 (fibs ⊕ fibs')
0 : 1 : take 2 ((0:fibs') ⊕ (1 : (fibs ⊕ fibs')))
0 : 1 : take 2 (1 : fibs' ⊕ (fibs ⊕ fibs'))
0 : 1 : 1 : take 1 (fibs' ⊕ (fibs ⊕ fibs'))
-}

-- exercícios:
-- números primos (primes)
-- primes = filterprimes [2..]
-- ^a partir de uma lista de números candidatos, pegar apenas os primos, o primeiro número é primo, pegar os múltiplos desse primo
-- hammin numbers
-- 1 é um HN, se x é HN 2x 3x e 5x são HN
-- me dê a uma lista ordenada deles
