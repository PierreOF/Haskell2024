oddquicksort :: [Int] -> [Int]
oddquicksort [] = []
oddquicksort (x:xs) = oddquicksort left ++ [x] ++ oddquicksort right
    where
        left = [a | a <- xs, a < x && odd a]
        right = [a | a <- xs , a >= x && odd a]



main :: IO ()
main = do
    
    print (oddquicksort [1,4,3,9,2,7,5])