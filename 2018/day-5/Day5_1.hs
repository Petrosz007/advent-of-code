{-# LANGUAGE BangPatterns #-}

module Day5_1 where

import Data.Function
import Data.Char
import Data.List
import System.IO.Unsafe
import Control.Parallel.Strategies

printLen :: String -> String
printLen a = b where
    !c = unsafePerformIO $ print $ length a
    b = a 

main :: IO()
main = do
        fileStr <- readFile "tibi.txt"
        print $  length $ fixPoint (filterAdjacent) fileStr
        let combinations = map (\x-> filter (\y->(toUpper y) /= x) fileStr) ['A'..'Z']
        print $ head $ sort $ parMap rdeepseq (length . fixPoint (filterAdjacent)) combinations
        

txt = "dabAcCaCBAcCcaDA"

filterAdjacent :: String -> String
filterAdjacent [] = []
filterAdjacent [x]         = [x] 
filterAdjacent (x:xs@(y:ys))
    | isPair x y = filterAdjacent ys
    | otherwise  = x : filterAdjacent xs

isPair :: Char -> Char -> Bool
isPair x y
    | isUpper x && isLower y = x == toUpper y
    | isLower x && isUpper y = toUpper x == y
    | otherwise              = False

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x = fun f x (f x) where
  fun f x y
    | x == y    = y
    | otherwise = fun f y (f y)

    --map (\x-> flip filter (\y->(toUpper y) /= x) txt) ['a'..'z']