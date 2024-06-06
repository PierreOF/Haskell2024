

fatorial :: Int -> Int
fatorial 1 = 1
fatorial a 
    | a > 0 = a * fatorial (a-1)
    | otherwise = error "numeros negativos nao sao válidos"

somar :: Int -> Int 
somar 0 = 0
somar x = x + somar (x-1)

expo :: (Eq a, Num a ) => a -> a -> a
expo a 1 = a
expo a b =  a * expo a (b-1)

euclides :: Int -> Int -> Int 
euclides a b
    | a == b = a
    | a > b = euclides (a-b) b
    | otherwise = euclides (b-a) a

and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs)
    | x = and2 xs
    | otherwise = False

-----  UTILIZANDO RECURSÃO ------------------- 
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ (concat2 xs)

-- utilizando foldr

concatfoldr :: [[a]] -> [a]
concatfoldr [] = []
concatfoldr (x:xs ) = foldl (++) x xs

-----------------------------------------------

replicate2 :: Int -> a -> [a]
replicate2 0 _ = []
replicate2 x a = a : replicate2 (x-1) a

eNesimo :: [a] -> Int -> a
eNesimo [] _ = error "lista vazia"
eNesimo (x:xs) pos
    | pos == 0 = x
    | otherwise =  eNesimo xs (pos-1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 a (x:xs) 
    | x == a = True
    | otherwise = elem2 a xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

metades :: [a] -> ([a],[a])
metades xs = splitAt ( div (length xs) 2 ) xs

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort xs = merge (mergesort ys) (mergesort zs)
    where
        (ys, zs) = metades xs

somaListInt :: [Int] -> Int
somaListInt [] = 0
somaListInt xs = head xs + somaListInt (tail xs)

numElems :: [a] -> Int
numElems [] = 0
numElems xs = 1 + numElems (tail xs)

lastElem :: [a] -> a
lastElem [] = error "empty list"
lastElem [a] = a
lastElem xs = lastElem (tail xs) 

main :: IO ()
main = do

    print (fatorial 5)

    print (somar 4)

    print (expo 1 4)

    print (euclides 6 27)

    print (and2 [True,True,False])

    print (concat2 [[1],[2],[4]])

    print (concatfoldr [[1],[2],[4]])    

    print (replicate2 2 "a")

    print (eNesimo [2,3,4,5] 2)

    print (elem2 2 [1,3,4])

    print (merge [2,5,6] [1,3,4])

    print (mergesort [3,1,2,3,4,5])

    print (somaListInt [1,2,3,4])

    print (numElems [1,2,3,4,5])

    print (lastElem [1,2,3,44])