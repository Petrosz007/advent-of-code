module Day_1_1 where

import Data.Char

main :: IO ()
main = do
    fileStr <- readFile "input-1-1.txt"
    putStrLn . show . sum . parseFileStr $ fileStr

parseFileStr :: String -> [Int]
parseFileStr = map fun . map (filter (/= '\r')) . lines where
    fun ('+':xs) = read xs
    fun ('-':xs) = - read xs