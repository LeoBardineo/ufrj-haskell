{-
cap 16

reverse [] = []                     (1)
reverse (x:xs) = reverse xs ++ [x]  (2)
[] ++ ys = ys                       (1)
(x:xs) ++ ys = x : (xs ++ ys)       (2)

(c é CAL (?), que é para significar qualquer tipo de lista)
prove que
    para todo C
    para toda expr x : C
    vale reverse [x] = [x]
resolução:
reverse [x]
= {reverse 2} -> precisa saber que [x] é não vazio
reverse [] ++ [x]
= {reverse 1} -> precisa saber que [] é vazio
[] ++ [x]
= {append 1} -> precisa saber que [] é vazio
[x]

data Bool = True | False
not True = False        (1)
not False = True        (2)

prove que not (not x) = x
resolução:
prova por casos:
    para provar que not (not x) = x
    provaremos que not (not True) = True e not (not False) = False pela def do tipo Bool
not (not True)
= {not 1}
not False
= {not 2}
True

not (not False)
= {not 2}
not True
= {not 1}
False

data Nat = Zero | Suc Nat
add n Zero = n                  (1)
add n (Suc m) = Suc(add n m)    (2)
    OU: add n (Suc m) = add Suc(n) m

prove que add Zero m = m
resolução:
provar por casos em m
add Zero Zero
= {add 1}
Zero

add Zero (Suc m')
= {add 2}
Suc (add m' Zero)

essa é uma prova por indução em m
para provar ∀m.P(m)
prove {
    P(zero)                 -> caso base
    ∀m(P(m) => P(Suc m))    -> caso indutivo
}
no nosso caso,
P(m) := add m Zero = m

No caso zero
queremos provar add Zero Zero = Zero
add Zero Zero
= {add 1}
Zero

(o m' é só porque ele é o cara de dentro)
No caso Suc m'
queremos provar add (Suc m') Zero = Suc m'
assumindo que a hipotese de indução add m Zero = m
add (Suc m') Zero
= {add 2}
Suc (add m' Zero)
= {hip indoução}
Suc m'

Portanto, para todo m, add m Zero = m

-}

