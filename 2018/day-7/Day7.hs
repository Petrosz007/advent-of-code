module Day7 where

import Data.Char
import Data.List

main :: IO()
main = do
        fileStr <- readFile "test.txt"
        print $ parseStr2 fileStr

parseStr :: String -> [(Char, Char)]
parseStr = map (\(x:y:[])->(x,y)) . map (filter isUpper) . map init . map tail . sort . lines

parseStr2 :: String -> [String]
parseStr2 = map (filter isUpper) . map init . map tail . sort . lines

machine :: String -> [(Char, Char)] -> String
machine acc []           = acc
machine acc (x@(a,b):xs) = undefined