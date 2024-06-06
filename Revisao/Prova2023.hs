dependentes :: [(a,[b])] -> [b]
dependentes lista = concat [ y | (_,y) <- lista ]

menorElem :: [Int] -> [Int] 
menorElem xs = deleti xs (minimum xs) 

deleti :: [Int] -> Int -> [Int]
deleti (x:xs) y 
    | y == x = xs
    | otherwise = x : deleti xs y 

horario :: Int -> String
horario int 
    | int < 359999 = show hora ++ ":" ++ show minuto ++ ":" ++ show segundo
    | otherwise = error "numero maior que o permitido"
    where
        hora = div int 3600
        minuto = div (mod int 3600) 60 
        segundo = mod (mod int 3600) 60

funcao :: [a -> Bool] -> a -> [a -> Bool]
funcao funcs x = filter (\f -> not (f x)) funcs

main :: IO ()
main = do
    print (dependentes [("joão",["maria","jeova"]),("teresa",["jeoviroDois","ze"])])

    print (menorElem [1,2,3,4,1,4,42,2])

    print (horario 86399)

    -- print (funcao [odd,\x -> mod x 3 == 0] 5) 