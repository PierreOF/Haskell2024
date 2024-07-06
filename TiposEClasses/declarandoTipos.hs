

data Nat = Zero | Suc Nat deriving Show

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Suc (int2nat (n-1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc n) = 1 + nat2int n

somar :: Nat -> Nat -> Nat
somar m n = int2nat (nat2int m + nat2int n)

-- Podemos considerar agora algumas funções em árvores:

data Arvore a = Folha a | No (Arvore a) a (Arvore a)

existe :: Ord a => a -> Arvore a -> Bool
existe x (Folha y) = x == y
existe x (No esq y dir)
    | x == y    = True
    | x < y     = existe x esq
    | otherwise = existe x dir


--IMPORTANTE: apenas tipos definidos utilizando-se data e newtype podem ser instâncias de classes.
--Definições padrão, como o que acontece com o operador /= podem ser 
--sobrescritos se for a vontade do programador.

main :: IO()
main = do

    print(somar Zero (Suc Zero))
