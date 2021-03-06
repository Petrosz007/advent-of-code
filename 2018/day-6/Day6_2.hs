{-# LANGUAGE BangPatterns #-}
module Day6_2 where

import Data.List
import Data.Char
import System.IO.Unsafe
import GHC.Exts
import Control.Parallel.Strategies

print2 :: Show a => a -> a
print2 a = b where
    !c = unsafePerformIO $ putStrLn $ show a
    b = a

type Point = (Int, Int)
type Id = Int 

main :: IO()
main = do
        fileStr <- readFile "input.txt"
        let points = parseStr $ fileStr
        print . calGoodArea . parMap rdeepseq (flip colorPoint points) $ genPts 400
        -- print . calGoodArea . map (flip colorPoint points) $ genPts 400

colorPoint :: (Id, Point) -> [(Id, Point)] -> (Bool, Point)
colorPoint (id,p) pts = (0 < (foldl fun 10000 pts), p) where
    fun :: Int -> (Id, Point) -> Int
    fun acc (_, pp) = acc - distance p pp

calGoodArea :: [(Bool, Point)] -> Int
calGoodArea = length . filter (==True) . map fst

deleteEdges :: [(Id, Point)] -> [(Id, Point)]
deleteEdges pts = filter (\(id,_)-> not $ elem id ids) pts where
    edge = snd $ last pts
    a = 0
    b = snd edge
    c = fst edge
    abc = [a,b,c]
    ids = nub . map fst . filter (\(id, (x, y))-> elem x abc || elem y abc) $ pts

calculateArea :: [(Id, Point)] -> [(Id, Int)]
calculateArea = sortWith snd . map (\xxs@(x:xs)-> (x, length xxs)) . group . sort . map fst

distance :: Point -> Point -> Int
distance (ax, ay) (bx, by) = abs x + abs y where
    x = ax - bx
    y = ay - by

roundDown :: Double -> Int
roundDown = read . head . words . map (\x-> if x == '.' then ' ' else x) . show

genPts :: Int -> [(Id, Point)]
genPts n = map (\(x,y)-> (-1, (x, y))) $ zipCombined [0..n] [0..n]

zipCombined :: [a] -> [a] -> [(a,a)]
zipCombined [] _ = []
zipCombined _ [] = [] 
zipCombined (x:xs) list = zip (repeat x) list ++ zipCombined xs list

parseStr :: String -> [(Id, Point)]
parseStr = zip [0..] . map (\(x:y:[])-> (read x, read y)) . filterStr

filterStr :: String -> [[String]]
filterStr = map words . lines . filter (\x-> x /='\r' && x /= ',')

printMap :: [(Bool, Point)] -> [String]
printMap pts = map (concatMap fun) . map (\x-> filter (\(_, (_, y))-> x == y) pts) $ [0..10] where
    fun (True,_) = "#"
    fun _        = " "