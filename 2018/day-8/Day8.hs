{-# LANGUAGE BangPatterns #-}
module Day8 where

import System.IO.Unsafe
import Data.Char

print2 :: Show a => a -> a
print2 a = b where
    !c = unsafePerformIO $ putStrLn $ show a
    b = a

main :: IO()
main = do
        fileStr <- readFile "input.txt"
        print . solve1 $ fileStr

parseStr :: String -> [Int]
parseStr = map read . words

machine :: [Int] -> [Int] -> Int -> [Int] -> Int 
machine _ _ acc [] = acc
machine [] [] acc (x:y:xs) = machine [x] [y] acc xs
machine (0:c:cs) (m:ms) acc xs = machine ((c-1):cs) ms (acc + sum ({-print2 $-} take m xs)) (drop m xs)
machine (0:cs) (m:ms) acc xs = machine cs ms (acc + sum ({-print2 $-} take m xs)) (drop m xs)
machine cs ms acc (x:y:xs) = machine (x:cs) (y:ms) acc xs 

solve1 :: String -> Int
solve1 = machine [] [] 0 . parseStr



test :: [Int]
test = [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]

machine2 :: [Int] -> [Int] -> [Int] -> Int -> [Int] -> Int 
machine2 _ _ _ acc [] = acc
machine2 [] [] [] acc (x:y:xs) = machine2 [x] [x] [y] acc xs
machine2 (0:c:cs) (og:ogs) (m:ms) acc xs = machine2 ((c-1):cs) (print2 $ og:ogs) ms (acc + sum ({-print2 $-} take m xs)) (drop m xs)
machine2 (0:cs) (og:ogs) (m:ms) acc xs = machine2 cs (print2 $ ogs) ms (acc + sum ({-print2 $-} take m xs)) (drop m xs)
machine2 cs ogs ms acc (x:y:xs) = machine2 (x:cs) (print2 $ (x:ogs)) (y:ms) acc xs 