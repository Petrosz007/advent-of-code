{-# LANGUAGE BangPatterns #-}

module Day3 where

import Data.Char
import System.IO.Unsafe

type Point = ((Int,Int), Int)

print2 :: Show a => a -> a
print2 a = b where
    !c = unsafePerformIO $ putStrLn $ show a
    b = a

main :: IO()
main = do
        fileStr <- readFile "input.txt"
        let max1 = foldl fun (0,0) . parseStr $ fileStr
        let max2 = foldl fun2 (0,0) . parseStr $ fileStr 
        let maximum = max (fst max1 + snd max1) (fst max2 + snd max2)
        print . length . print2 . filter (\(_,x)-> x>=2) . foldl addToPoints (pointList maximum) . parseStr $ fileStr
        where
            fun (a,b) (_,(x,_),(z,_))
                | x + z > a + b = (x,z)
                | otherwise = (a,b)
            fun2 (a,b) (_,(_,x),(_,z))
                | x + z > a + b = (x,z)
                | otherwise = (a,b)

solve1 :: String -> Int
solve1 = undefined

addToPoints :: [Point] -> (Int, (Int, Int), (Int, Int)) -> [Point]
addToPoints ps (_,(left,top),(width,height)) = map incPoint ps where
    topLeft = (left, top)
    bottomRight = (left + width, top + height)
    incPoint p@((x,y),n)
        | (x >= left && x < left + width) &&
          (y >= top && y < top + height) = ((x,y),n+1)
        | otherwise = p

pointList :: Int -> [Point]
pointList n = map (\(x,y)-> ((x,y),0)) $ zipCombined [1..n] [1..n]

zipCombined :: [a] -> [a] -> [(a,a)]
zipCombined [] _ = []
zipCombined _ [] = [] 
zipCombined (x:xs) list = zip (repeat x) list ++ zipCombined xs list

parseStr :: String -> [(Int, (Int, Int), (Int, Int))]
parseStr = map fun2 . lines . filter fun where
    fun '\r' = False
    fun '#' = False
    fun '@' = False
    fun ':' = False
    fun _ = True
    fun2 =  fun3 . map read . words . map (\x-> if x == ',' || x == 'x' then ' ' else x)
    fun3 (id:x:y:a:b:[]) = (id,(x,y),(a,b))