

primeira :: Int 
primeira = sum [x * x | x <- [1..100]] 

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(a,b) | a <- [0..x] , b <- [0..y]]

quadrado :: Int -> [(Int,Int)]
quadrado x = [(a,b) | (a,b) <- grid x x , b/=a]

replicato :: Int -> a -> [a]
replicato int a = [ a | x <- [1..int]]

pitag :: Int -> [(Int, Int, Int)]
pitag x = [ (a,b,c) | a <- [1..x] , b <- [1..x], c <- [1..x], a*a + b*b == c*c ] 

fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. n], mod n x == 0]

perfeitos :: Int -> [Int]
perfeitos int = [x | x <- [0..int],x == sum (fatores x) - x ]

setima :: [(Int,Int)]
setima = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]


buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']

posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = [i | i <- buscar x (zip xs [0..] )]

pescalar :: [Int] -> [Int] -> Int
pescalar xs ys =  sum [ a * b | (a,b) <- zip xs ys ]  

main :: IO ()
main = do

    print (pescalar [1,2,3] [1,2,3])
    
    print primeira

    print (grid 1 2)

    print (quadrado 2)

    print (replicato 3 True)

    print (pitag 10)

    print (perfeitos 500)

    print setima

    print (posicoes 1 [1,2,3,4,1,4,1])

