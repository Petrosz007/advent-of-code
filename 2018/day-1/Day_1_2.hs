module Day_1_2 where

import Data.Char
import qualified Data.Set as Set

main :: IO ()
main = do
    fileStr <- readFile "input-1-2.txt"
    let parsedInts = parseFileStr $ fileStr
    putStrLn . show . machine2 . sumStr $ (cycle parsedInts)

parseFileStr :: String -> [Int]
parseFileStr = map fun . map (filter (/= '\r')) . lines where
    fun ('+':xs) = read xs
    fun ('-':xs) = - read xs

sumStr :: [Int] -> [Int]
sumStr = scanl (+) 0

machine :: [Int] -> [Int] -> Int
machine acc (x:xs)
    | elem x acc = x
    | otherwise = machine (x:acc) xs


machine2 :: [Int] -> Int
machine2 = head . head . filter (not . null) . map fst . scanl fun ([],[]) where
    fun (dup, acc) x
        | elem x acc = (dup ++ [x], acc)
        | otherwise = (dup, x:acc)
