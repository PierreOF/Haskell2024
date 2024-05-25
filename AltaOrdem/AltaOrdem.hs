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

main :: IO ()
main = do

--    print (oddquicksort [1,4,3,9,2,7,5])
    --print (nEsima 1 [1,2])
    --print (replicato 4)
    --print (palindromo [1,2,2])
    -- print (fibo 10)
    -- print (todos (<10) [1,2,1])
    -- print (algum (<10) [11,11,230])
    -- print (pegueEnquanto (<10) [11,21,13,11,12])

    print (dropEnquanto (<10) [1,2,3,10,11])

    