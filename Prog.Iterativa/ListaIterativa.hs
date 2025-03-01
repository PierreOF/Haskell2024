module Lib
    ( putStr', acumular, somador, somador2, obterLinha
    ) where

import Text.Printf
import System.IO

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

somador :: IO ()
somador = do putStr "Quantos números? "
             n <- readLn :: IO Int
             soma <- acumular n 0
             putStr $ printf "O total é %d" soma

acumular :: Int -> Int -> IO Int
acumular 0 acc = return acc
acumular n acc = do x <- readLn :: IO Int
                    acumular (n - 1) (x + acc)

somador2 :: IO ()
somador2 = do putStr "Quantos números? "
              n <- readLn :: IO Int
              ns <- sequence [readLn :: IO Int | _ <- [1 .. n]]
              putStr $ show $ sum ns


obterChar:: IO Char
obterChar = do hSetEcho stdin False
               x <- getChar
               hSetEcho stdin True
               return x

obterLinha :: IO String
obterLinha = do x <- obterChar
                if x == '\n' then
                    do putChar x
                       return []
                else
                    if x == '\DEL' then
                        do putStr "\b \b"
                           return "\DEL"
                    else
                        do putChar x
                           xs <- obterLinha
                           if xs == "\DEL" then
                              do obterLinha
                           else
                              return (x:xs)


-- Função para acumular a multiplicação
multiplicar :: Int -> Int -> IO Int
multiplicar 0 acc = return acc
multiplicar n acc = do
  x <- readLn :: IO Int
  multiplicar (n - 1) (x * acc)

-- Função principal para ler os números e multiplicar
multiplicador :: IO ()
multiplicador = do
  putStr "Quantos números? "
  n <- readLn :: IO Int
  resultado <- multiplicar n 1
  putStrLn $ "Resultado da multiplicação: " ++ show resultado

















