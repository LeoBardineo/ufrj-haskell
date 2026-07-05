livro de semântica: winskel
numerais de church
I K B C S W T Y combinators

0 = \f.\x.x
1 = \f.\x.f(x)
N = \f.\x.f^N(x)

suc:: Nat -> Nat
suc = \n.\f.\x.f(nfx)
add = \n\m.\fx.(mf)(nfx)
mul = \n\m.\fx.m(nfx)x

construtores e destrutores (Pares de Church)
par = \xy . \f . f x y
\pi_1 = \p . p(\x . \y . x)
\pi_2 = \p . p(\x . \y . y)

\pi_1(par A B)
= \pi_1 (\f . f A B)
= (\f . f A B) (\x y . x)
= (\x y . x) A B
= A

booleanas
TRUE = \x y . x
FALSE = \x y . y
IFELSE = \b x y . b x y

IF B THEN
	X
ELSE
	Y

antecessor

fixo :: (a -> a) -> a
fixo f = f (fixo f)

mkfac :: (Integer -> Integer) -> (Integer -> Integer)
mkfac = \fac -> (\n -> if n == 0 then 1 else n * fac(n-1))

facs :: [Integer -> Integer]
facs = iterate mkfac undefined

superfac :: (Integer -> Integer)
superfac = fixo mkfac

{-
superfac 5
= (fixo mkfac) 5
= mkfac (fixo mkfac) 5
= 5 * (fixo mkfac) 4
= 5 * mkfac (fixo mkfac) 4
= 5 * 4 * (fixo mkfac) 3
= 5 * 4 * 3 * (fixo mkfac) 2
= 5 * 4 * 3 * 2 * (fixo mkfac) 1
= 5 * 4 * 3 * 2 * 1 * (fixo mkfac) 0
= 5 * 4 * 3 * 2 * 1 * mkfac (fixo mkfac) 0
= 5 * 4 * 3 * 2 * 1 * 1
-}


