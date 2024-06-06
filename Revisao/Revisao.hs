addTuplesToList :: Num b => [(b, b)] -> [b]
addTuplesToList = map (\(x,y) -> x + y) 

tupleToList :: [(a,a)] -> [a]
tupleToList = concatMap (\(x,y) -> [x,y]) 

tupleToListLC :: [(a, a)] -> [a]
tupleToListLC tuples = [item | (x, y) <- tuples, item <- [x, y]]

-- Exercicio 3
splitString :: Int -> Int -> [a] -> [a]
splitString initial final list = drop (initial-1) (take final list)

ordenaImpar :: [Int]-> [Int]
ordenaImpar [] = []
ordenaImpar (x:xs) = ordenaImpar left ++ [x] ++ ordenaImpar right
    where 
        left = [ a | a <- xs , (a < x) && odd a]
        right = [a | a <- xs , (a >= x) && odd a]

meuSplit :: String -> Char -> [String]
meuSplit [] _ = []
meuSplit texto x = takeWhile (/= x) texto : meuSplit (drop 1 (dropWhile (/= x) texto)) x


iguais :: Eq a => [a] -> Bool
iguais [] = True
iguais (x:xs)
    | x == head xs = iguais (tail xs)
    | otherwise = False

palindromo :: Eq a => [a] -> Bool
palindromo [] = True
palindromo [a] = True
palindromo (x:xs)
    | x == last xs = palindromo $ take (length xs - 1) xs
    | otherwise = False


teste :: [Int] -> [Int]
teste [] = []
teste (x:xs)
    | x == minimum1 (x:xs) = xs
    | otherwise = x : teste xs


minimum1 :: Num a => [a] -> a
minimum1 [a] = a
minimum1 (x:xs)
    | head xs < x = minimum1 xs 
    | otherwise = minimum1 x : initial xs

    main :: IO ()
main = do

    print (teste [1,4,5,6,7])

    print (palindromo [1,2,3,2,1])
    print (palindromo [1,2,3,22,1])

    print (iguais [1,1,1,1])
    print (iguais [1,1,1,3])

    print (meuSplit "bom dia ola mundo" ' ')

    -- print (ordenaImpar [5,4,3,2,1] )

    print (splitString 2 5 "adauberto")

-- 1 2 3 4 5 6 7 8 
-- a d a u b e r t o

    print (addTuplesToList [(1,2),(2,4)] )

    print (tupleToList [(1,2),(2,8)])

    print (tupleToListLC [(1,2),(3,8)])
