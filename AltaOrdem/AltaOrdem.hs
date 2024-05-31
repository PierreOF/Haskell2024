oddquicksort :: [Int] -> [Int]
oddquicksort [] = []
oddquicksort (x:xs) = oddquicksort left ++ [x] ++ oddquicksort right
    where
        left = [a | a <- xs, a < x && odd a]
        right = [a | a <- xs , a >= x && odd a]

nEsima :: Int -> [a] -> a
nEsima pos (x:xs)
    | length (x:xs) - 1 < pos = error "a lista enviada não possui essa posição"
    | pos == 0 = x
    | otherwise = nEsima (pos - 1) xs

replicato :: Int -> [Int]
replicato num
    | num == 0 = []
    | otherwise = take (num) (repeat num) ++ replicato (num-1)

palindromo :: Eq a => [a] -> Bool
palindromo [a] = True
palindromo [] = True
palindromo (x:xs)
    | x /= last xs = False
    | otherwise = palindromo (take (length xs - 1) xs)

fibo :: Int -> [Int] 
fibo n = take n fibs
    where 
        fibs = 0 : 1 : [a + b | (a,b) <- zip fibs (tail fibs)]


todos :: (a->Bool) -> [a] -> Bool
todos _ [] = True
todos predicado (x:xs)
    | not (predicado x) = False
    | otherwise = todos predicado xs

algum :: (a -> Bool) -> [a] -> Bool
algum _ [] = False
algum predicado (x:xs)
    | predicado x = True
    | otherwise = algum predicado xs

pegueEnquanto :: (a->Bool) -> [a] -> [a]
pegueEnquanto _ [] = []
pegueEnquanto predicado (x:xs)
    | not (predicado x) = []
    | otherwise = x : pegueEnquanto predicado xs

dropEnquanto :: (a -> Bool) -> [a] -> [a]
dropEnquanto _ [] = []
dropEnquanto pred (x:xs)
    | not (pred x) = x : dropEnquanto pred xs
    | otherwise = dropEnquanto pred xs


-- Redefina as funções map e filter usando foldr
mapRedefined :: (a -> b) -> [a] -> [b]
mapRedefined pred = foldr (\x acc -> pred x : acc) [] 

filterRedefined :: (a -> Bool) -> [a] -> [a]
filterRedefined pred = foldr (\x acc -> if pred x then x : acc else acc) [] 

dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> 10 * acc + x) 0 


unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x 
       | p x = []
       | otherwise = h x : unfold p h t (t x)


-- se o 1º predicado for True -> Lista vazia

-- senão h -> x (cabeça) + t -> x (cauda)

-- 1 0 1 1 1

mapunfold :: (t -> a) -> [t] -> [a]
mapunfold f = unfold null (f . head) tail

iteunfold :: (a -> a) -> a -> [a]
iteunfold f = unfold (const False) id f

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap a b (x:xs) = a x : altMap b a xs 
     
-- curry :: (a -> b) -> (a->b, a->b)
-- curry f g =

-- uncurry :: (a->b, a->b) -> (a -> b)
-- uncurry f g =



main :: IO ()
main = do
     print (altMap (2*) (0*) [1,2,3,4,5])
    -- print (take 10 (iteunfold (+1) 1))
    -- print (unfold (== 0) (`mod` 2) (`div` 2) 29)
    -- print (oddquicksort [1,4,3,9,2,7,5])
    -- print (nEsima 1 [1,2])
    -- print (replicato 4)
    -- print (palindromo [1,2,2])
    -- print (fibo 10)
    -- print (todos (<10) [1,2,1])
    -- print (algum (<10) [11,11,230])
    -- print (pegueEnquanto (<10) [11,21,13,11,12])
    -- print (dropEnquanto (<10) [1,2,3,10,11])
    -- print (mapRedefined reverse ["dsaa","123","321"])
    -- print (filterRedefined (odd) [10,1,11,2])

    -- print (dec2int [1,2,3,4])


    