{-# LANGUAGE BangPatterns, DeriveGeneric #-}

module Day18 where

import qualified Data.Map.Lazy as M
import Control.Parallel.Strategies
import Control.DeepSeq.Generics (genericRnf)
import GHC.Generics
import Data.List
import System.IO.Unsafe

type Point = (Int,Int)

data Acre = Open | Tree | Yard
    deriving(Eq, Enum, Generic)

type Board = M.Map Point Acre

instance Show Acre where
    show x = [toChar x]

instance NFData Acre

toChar :: Acre -> Char
toChar Open = '.'
toChar Tree = '|'
toChar Yard = '#'

toAcre :: Char -> Acre
toAcre '.' = Open
toAcre '|' = Tree
toAcre '#' = Yard

main :: IO()
main = do
    fileStr <- readFile "input.txt"
    let board = parseStr $ fileStr
    -- putStrLn $ show $ (filter (/='\r') fileStr) == stringBoard board
    -- putStrLn . stringBoard . map (stepAcre board) $ board
    -- print $ solve1 fileStr
    -- let finalBoard = step2 board
    -- putStrLn $ stringBoard $ M.toList $ finalBoard
    -- print $ solveBoard finalBoard
    print . step3 . parseStr $ fileStr

unsafeFileStr :: String
unsafeFileStr = unsafePerformIO $ readFile "input.txt"

parseStr :: String -> Board
parseStr = M.fromList . snd . foldl fun (0,[]) . lines . filter (/='\r') where
    fun :: (Int, [(Point, Acre)]) -> String -> (Int, [(Point, Acre)])
    fun (n,xs) s = (n+1, xs ++ (snd  (foldl (fun2 n) (0,[]) s)))
    fun2 :: Int -> (Int, [(Point, Acre)]) -> Char -> (Int, [(Point, Acre)])
    fun2 n (j,xs) acre = (j+1, xs ++ [((n,j), toAcre acre)])

stringBoard :: [(Point, Acre)] -> String
stringBoard xs = intercalate "\n" $ map (\x-> map (toChar . snd) $ filter (\((y,_),_)-> x == y) xs) [0..numRows]  where
    numRows = head . reverse . sort . map fst $ map fst xs

adjacentsPoints :: Point -> [Point]
adjacentsPoints (x,y) = [(x-1,y-1), (x-1,y), (x-1,y+1),
                         (x,y-1), (x,y+1),
                         (x+1,y-1), (x+1,y), (x+1,y+1)]

getAdjacents :: Board -> (Point, Acre) -> [Acre]
getAdjacents board (p, _) = concat . filter (not . null) . parMap rdeepseq fun $ adjacentsPoints p where
    fun x = case M.lookup x board of
            Nothing -> []
            Just x -> [x]


rule1 :: Board -> (Point, Acre) -> Bool
rule1 board p@((x,y), Open)
    | length (filter (==Tree) $ getAdjacents board p) >= 3 = True
    | otherwise = False
rule1 _ _ = False

rule2 :: Board -> (Point, Acre) -> Bool
rule2 board p@((x,y), Tree)
    | length (filter (==Yard) $ getAdjacents board p) >= 3 = True
    | otherwise = False
rule2 _ _ = False

rule3 :: Board -> (Point, Acre) -> Bool
rule3 board p@((x,y), Yard)
    | a && b = True 
    | otherwise = False
    where
        a = length (filter (==Yard) $ getAdjacents board p) >= 1
        b = length (filter (==Tree) $ getAdjacents board p) >= 1
rule3 _ _ = False


stepAcre :: Board -> (Point, Acre) -> (Point, Acre)
stepAcre board x@(p, acre)
    | rule1 board x                         = (p, Tree)
    | rule2 board x                         = (p, Yard)
    | rule3 board x                         = (p, Yard)
    | (not $ rule3 board x) && acre == Yard = (p, Open)
    | otherwise                             = (p, acre)

step :: Int -> Board -> Board
step n x = snd $ until (\(x,_)-> x <= 0) fun (n, x) where
    fun (num, prev) = (num-1, M.fromList . parMap rdeepseq (stepAcre prev) . M.toList $ prev)

step2 :: Board -> Board
step2 x = fixPoint fun x where
    fun prev = M.fromList . print3 . parMap rdeepseq (stepAcre prev) . M.toList $ prev

solve1 :: String -> Int
solve1 s = trees * yards where
    board = step 10 $ parseStr s
    trees = length . filter (== Tree) . M.elems $ board
    yards = length . filter (== Yard) . M.elems $ board

solveBoard :: Board -> Int
solveBoard board = trees * yards where
    trees = length . filter (== Tree) . M.elems $ board
    yards = length . filter (== Yard) . M.elems $ board


step3 :: Board -> ((Int, Int), Board)
step3 x = fixPoint fun ((-1, 0), x) where
    fun ((n, sum), prev) = (print2 $ (n+1, solveBoard prev), M.fromList . parMap rdeepseq (stepAcre prev) . M.toList $ prev)

print2 :: Show a => a -> a
print2 a = b where
    !c = unsafePerformIO . print . show $ a
    b = a 

print3 :: [(Point, Acre)] -> [(Point, Acre)]
print3 a = b where
    !c = unsafePerformIO . putStrLn $ (stringBoard a ++ "\n\n")
    b = a 

fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f x = fun f x (f x) where
  fun f x y
    | x == y    = y
    | otherwise = fun f y (f y)