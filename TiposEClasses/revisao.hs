data Nat = Zero | Suc Nat deriving Show


somar :: Nat -> Nat -> Nat
somar Zero n = n
somar (Suc m) n = Suc (somar m n)


mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult x (Suc y) = somar x (mult x y)

-- 2 4 = somar 2 (mult 2 3)

data Arvore a = Folha a | No (Arvore a) a (Arvore a)

existe :: Ord a => a -> Arvore a -> Bool
existe x (No esq y dir)
    | compare x y == EQ || existe x esq || existe x dir = True
    | otherwise = False

-- 3 questao

contarFolhas :: Arvore a -> Int
contarFolhas (Folha a) = 0
contarFolhas (No esq x dir) = 1 + contarFolhas esq + contarFolhas dir

balanceada :: Arvore a -> Bool
balanceada (No esq x dir) = contarFolhas esq == contarFolhas dir 

-- 4 questao

divideLista :: [a] -> ([a], [a])
divideLista lista = splitAt (length lista `div` 2) lista

balancear :: [a] -> Arvore a
balancear [a] = Folha a
balancear lista = No (balancear esq) meio (balancear dir)
  where
    (esqLista, meio:dirLista) = divideLista lista
    esq = esqLista
    dir = dirLista


-- balancear :: [a] -> Arvore a


-- 2 * 2  == 2 + 2 

data Expr = Val Int | Add Expr Expr


avaliar :: Expr -> Int
avaliar = 





main :: IO ()
main = do

    print(mult (Suc(Suc Zero)) (Suc(Suc(Suc Zero))) )

    print (balanceada (No (No (Folha 1) 3 (Folha 4)) 5 (No (Folha 2) 7 (Folha 9))))


