quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort left ++ [x] ++ quicksort right
    where
        left = [a | a <- xs, a < x && odd a]
        right = [a | a <- xs , a >= x && odd a]

main :: IO ()
main = do
    
    print (quicksort [1,4,3,9,2,7,5])