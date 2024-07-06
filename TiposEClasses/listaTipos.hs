
-- Questões de haskell sobre tipos e classes
--  1 Questao

data Nat = Zero | Suc Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Suc (int2nat (n-1))

somar :: Nat -> Nat -> Nat
somar m n = int2nat (nat2int m + nat2int n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult x (Suc y) = somar x (mult x y) 

-- 2 Questão
data Arvore a = Folha a | No (Arvore a) a (Arvore a)

existe :: Ord a => a -> Arvore a -> Bool
existe x (No esq y dir)
    | compare x y == EQ || existe x esq || existe x dir = True
    | otherwise = False

--  3 Questão
contarFolhas :: Arvore a -> Int
contarFolhas (Folha a) = 1
contarFolhas (No esq x dir) = 0 + contarFolhas esq + contarFolhas dir

-- 4 Questao
balanceada :: Arvore a -> Bool
balanceada (No esq x dir)
    | contarFolhas esq == contarFolhas dir = True 
    | otherwise = False

-- 5 Questão

data Expr = Val Int | Add Expr Expr

avaliar :: Expr -> Int
avaliar (Val a) = a
avaliar (Add esq dir) = avaliar esq + avaliar dir

--  6 questão 

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val a) = f a
folde f g (Add esq dir) = g (folde f g esq) (folde f g dir)

-- 7 Questão

eval :: Expr -> Int
eval = folde id (+)


main :: IO ()
main = do

    -- print(mult (Suc(Suc Zero)) (Suc(Suc(Suc Zero))) )

    -- print (existe 2 (No (No (Folha 1) 3 (Folha 4)) 5 (No (Folha 6) 7 (Folha 9))))

    print (contarFolhas (No (No (Folha 1) 3 (Folha 4)) 5 (No (Folha 6) 7 (Folha 9))))

    print (balanceada (No (No (Folha 1) 3 (Folha 4)) 5 (No (No (Folha 0) 1 (Folha 2)) 7 (Folha 9))))

    print (avaliar (Add (Val 3) (Add (Val 4) (Val 5))))

    print (eval (Add (Val 1) (Val 4)))














