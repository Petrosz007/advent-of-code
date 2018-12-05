module Day2 where

import Data.List
import System.IO.Unsafe

main :: IO()
main = do
      fileStr <- readFile "bollash.txt"
      print . solve1 $ fileStr  
      print . solve2 $ fileStr  

solve1 :: String -> Int
solve1 = checksum . get2And3Count . parseInput

checksum :: (Int, Int) -> Int
checksum (x,y) = x * y

get2And3Count :: [String] -> (Int, Int)
get2And3Count = foldl fun (0,0) . filter (/=(False, False)) . map getTwosAndThrees where
    fun (x,y) (True, True)  = (x+1, y+1)
    fun (x,y) (True, False) = (x+1, y)
    fun (x,y) (False, True) = (x, y+1)
    fun acc _               = acc

getTwosAndThrees :: String -> (Bool, Bool)
getTwosAndThrees str = foldl fun (False, False) $ map length grouped where
    grouped = group $ sort str
    fun (_,y) 2 = (True, y)
    fun (x,_) 3 = (x, True)
    fun acc _   = acc





solve2 :: String -> [String]
solve2 = map snd . filter (\(x,_)-> x) . map correctBox . zipCombined . parseInput

correctBox :: (String, String) -> (Bool, String)
correctBox (x,y) = (lengthN ((length x) - 1) reduced, map snd reduced)  where
    reduced = filter (\(b,_)-> b) $ zipWith (\a b-> (a==b, a)) x y

zipCombined :: [a] -> [(a, a)]
zipCombined [] = [] 
zipCombined (x:xs) = map (\a->(x,a)) xs ++ zipCombined xs

lengthN ::  Int -> [a] -> Bool
lengthN n list = length list == n




fileStr :: String
fileStr = unsafePerformIO $ readFile "input.txt"

parseInput :: String -> [String]
parseInput =  lines . filter (/='\r')