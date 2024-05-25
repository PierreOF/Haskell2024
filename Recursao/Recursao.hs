


fatorial :: Int -> Int
fatorial int
    | int < 1 = error "não pode numero negativo"
    | int == 1 = 1
    | otherwise = int * fatorial  (int - 1)


somar :: Int -> Int 
somar int 
    | int == 1 = 1
    | otherwise = int + somar (int-1)

elevar :: (Num a , Eq a) => a -> a -> a
elevar m 0 = 1
elevar m n = m * elevar m (n - 1)
    
euclides :: Int -> Int -> Int
euclides a b 
    | a == b = a
    | a < b = euclides (b - a) a 
    | otherwise = euclides (a - b ) b

true :: [Bool] -> Bool
true [] = True
true (x:xs)  
   | not x = False
   | otherwise = true xs

concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs 

identicos :: Int -> a -> [a]
identicos int a
    | int == 0 = []
    | otherwise = identicos (int - 1) a ++ [a]

selecionar :: [a] -> Int -> a
selecionar (x:xs) pos 
    | pos == 0 = x
    | otherwise = selecionar xs (pos - 1)

decidir :: Eq a => a -> [a] -> Bool
decidir _ [] = False
decidir a (x:xs)
    | x == a = True
    | otherwise = decidir a xs


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x:merge xs (y:ys)
    | otherwise = y: merge (x:xs) ys

metades :: [a] -> ([a],[a])
metades xs = (take lhx xs , drop lhx xs)
    where lhx = length xs `div` 2

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort xs = merge (mergesort esquerda) (mergesort direita)
    where 
        (esquerda,direita) = metades xs

somainteiros :: [Int] -> Int
somainteiros [] = 0
somainteiros xs = foldr (+) 0 xs

numElements :: [a] -> Int
numElements [] = 0
numElements xs = 1 + numElements (drop 1 xs)

ultimoElem :: [a] -> a
ultimoElem [a] = a
ultimoElem xs = ultimoElem (tail xs)

main :: IO ()
main = do 

    --print (fatorial 1 )

    --print (somar 3)

    --print ( elevar 5 2)

    --print (euclides 6 27)

    --print (true [True,True] )

    --print (concatenar [["um"],["dois"],["tres"]])

    --print (identicos 3 "oi")

    --print (selecionar [1,2,3,4,5] 1)

    --print (decidir 5 [])

    --print (merge [1,4,5] [2,3,4])

    --print (mergesort [1,4,5,2,3])

    --print (numElements [1,2,3,4])

    --print(ultimoElem [1,2,3,4])

    print( somainteiros [-10,-20])
