

--  1º


reversi :: [a] -> [a]
reversi [] = []
reversi lista = last lista : reversi (take (length lista -1) lista)

-- 2 º

horario :: Int -> String
horario int 
    | int < 359999 = show hora ++ ":" ++ show minuto ++ ":" ++ show segundo
    | otherwise = error "numero maior que o permitido"
    where
        hora = div int 3600
        minuto = div (mod int 3600) 60 
        segundo = mod (mod int 3600) 60


-- 3º

rotateleft :: Int -> [a] -> [a]
rotateleft 0 lista = lista
rotateleft n lista = drop n lista ++ take n lista

--  4º

removeMin :: [Int] -> [Int]
removeMin [] = []
removeMin (x:xs) 
    | x == minimum (x:xs) = xs
    | otherwise = x : removeMin xs

-- 5º

funcao :: [a -> Bool] -> a -> [a -> Bool]
funcao [] _ = []
funcao (x:xs) n
    | x n == False = x : funcao xs n
    | otherwise = funcao xs n




main :: IO ()
main = do

    print (removeMin [2,3,4,5,1])
    print (rotateleft 11 "abcde" )
    print (reverse [1,2,3,4])









