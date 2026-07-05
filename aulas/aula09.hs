-- aula passada teve definição de tipo
-- rec: real world haskell
-- pesq: record, typeclasses

-- opção 1
-- type Cliente = (String, String, Bool)

-- hugo :: Cliente
-- hugo = ("Hugo", "000.000.000-00", False)

-- bom_dia :: Cliente -> String
-- bom_dia (nome, _, _) = "Olá, " ++ nome

-- problema 1:  ordem dos campos
--              campos não tem nome (1, 2, 3)
-- problema 2:  tupla são tipos estruturais e não nominais

-- opção 2
-- data Cliente = Cliente String String Bool

-- hugo = Cliente "Hugo" "000.000.000-00" False

-- bom_dia :: Cliente -> String
-- bom_dia (Cliente nome _ _) = "Olá, " ++ nome

-- também sem ordem dos campos
-- mas é tipagem nominal

-- opção 3
-- funções "getter" abstraem a ordem dos campos
-- ainda é chato que na hora que construir é preciso saber a ordem
-- data Cliente = Cliente String String Bool

-- nome :: Cliente -> String
-- nome (Cliente n _ _) = n

-- cpf :: Cliente -> String
-- cpf (Cliente _ c _) = c

-- devedor :: Cliente -> Bool
-- devedor (Cliente _ _ d) = d

-- hugo = Cliente "Hugo" "000.000.000-00" False

-- bom_dia :: Cliente -> String
-- bom_dia c = "Olá, " ++ nome c

-- opção 4
-- cliente_ no começo porque pode dar conflito com outras expressões de mesmo nome
-- data Cliente  = Cliente {
--     cliente_nome :: String,
--     cliente_cpf :: String,
--     cliente_devedor :: Bool
-- }

-- bom_dia :: Cliente -> String
-- bom_dia c = "Olá, " ++ nome c

-- hugo = Cliente {
--     cliente_nome = "Hugo",
--     cliente_cpf = "000.000.000-00",
--     cliente_devedor = False
-- }

-- opção 5
-- data Nome = Nome String
-- data CPF = CPF String

-- unNome :: Nome -> String
-- unNome (Nome s) = s

-- unCpf :: Cpf -> String
-- unCpf (Cpf s) = s

-- opção 6
-- data Nome = Nome { unNome :: String}
-- data CPF = CPF { unCpf :: String }

-- umnome = Nome "Hugo"
-- umcpf = CPF "000.000.00-00"

-- opção 7
newtype Nome = Nome { unNome :: String }
newtype Cpf = Cpf { unCpf :: String }

data Cliente  = Cliente {
    cliente_nome :: Nome,
    cliente_cpf :: Cpf,
    cliente_devedor :: Bool
}

bom_dia :: Cliente -> String
bom_dia c = "Olá, " ++ unNome (cliente_nome c)

hugo = Cliente {
    cliente_nome = Nome "Hugo",
    cliente_cpf = Cpf "000.000.000-00",
    cliente_devedor = False
}

--------------------------------------------

show_cpf :: Cpf -> String
show_cpf c = "Cpf{"++ unCpf c ++ "}"

instance (Show Cpf) where
    show = show_cpf cpf

instance (Show Nome) where 
    show n = "Nome{"++ unNome n ++"}"

--------------------------------------------

elemento :: a -> [a] -> Bool
elemento x [] = False
elemento x (y:ys) = x == y || elemento x ys

data EqMethods = a EqMethods {
    eq :: a -> a -> Bool
}

eq_bool :: Bool -> Bool -> Bool
eq_bool True True = True
eq_bool False False = True
eq_bool _ _ = False

eqmethods_bool :: EqMethods Bool
eqmethods_bool = EqMethods {eq = eq_bool}

eq_integer :: Integer -> Integer -> Bool
eq_integer a b = (a == b)

eqmethods_integer :: EqMethods Integer
eqmethods_integer = EqMethods {eq = eq_integer}

elemento :: EqMethods a -> a -> [a] -> Bool
elemento em x [] = False
elemento em x (y:ys) = (eq em) x == y || elemento em x ys

---------

data Point = Point Double Double
    deriving (Eq, Show, Read)

