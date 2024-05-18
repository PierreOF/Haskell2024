
--Usando compreensão de listas, forneça uma expressão que calcula a soma 
-- 1² +2²+...+100² dos quadrados dos primeiros 100 números inteiros.

-- 1 questao
soma :: Int -> Int
soma int = sum[x * x | x <- [1 .. int]]

-- 2 questao
grid :: Int -> Int -> [(Int,Int)]
grid um dois = [(x,y) | x <- [0 .. um] , y <- [0 .. dois]] 

-- 3 questao
quadrado :: Int -> [(Int,Int)]
quadrado int = [(x ,y) | (x ,y) <- grid int int, x /= y ]

-- 4 questao
replicateDois :: Int -> a -> [a]
replicateDois a b = [b | _ <- [1..a]]

-- 5 questao
pitag :: Int -> [(Int, Int, Int)]
pitag int = [(x,y,z) | x <- [1..int] , y <- [1..int] , z <- [1..int] , (x * x + y * y == z * z) && (z <= int)]

-- 6 questao
fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. div n 2], mod n x == 0]

perfeitos :: Int -> [Int]
perfeitos int = [x | x <- [2..int], sum (fatores x) == x]

-- 7 questão
provando :: [(Integer,Integer)]
provando = concat [[ (x,y) | x <- [3, 4]] | y <- [1,2]]

-- 8 questao

posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = buscar x (zip xs [0..])


buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']

-- 9 questoes 

produtoaa :: [Int] -> [Int] -> Int
produtoaa a b = sum [ x * y | (x,y) <- zip a b, x * y /= 0 ]

