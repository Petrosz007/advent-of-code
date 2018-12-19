{-# LANGUAGE BangPatterns #-}

module Day16 where

import Data.Bits
import Data.Maybe
import Data.List
import qualified Data.IntMap.Lazy as M
import System.IO.Unsafe


main :: IO()
main = do
        fileStr <- readFile "input1.txt"
        --print $ solve1 fileStr
        print . map (\(x,(y,_))->(x,y)) . M.toList . step M.empty [] . map getPossibleFuns . parseStr $ fileStr

parseStr :: String -> [([Int], [Int], [Int])]
parseStr = fun [] . ls where
    ls = filter (not . null) . lines . filter (not . flip elem ('\r':"Before:At[,]"))
    f :: String -> [Int]
    f = map read . words
    fun :: [([Int], [Int], [Int])] -> [String] -> [([Int], [Int], [Int])]
    fun acc [] = acc
    fun acc (x:y:z:xs) = fun (acc++[(f x, f y, f z)]) xs

-- FUNCTIONS --
-- addr, addi, multr, multi,  banr :: [Int] -> [Int] -> Int
addr = reg (+)
addi = imm (+)
multr = reg (*) 
multi = imm (*)
banr = reg (.&.)
bani = imm (.&.)
borr = reg (.|.)
bori = imm (.|.)
setr xs [x,_] = xs!!x
seti _ [x,_] = x
gtir = immRegB (>)
gtri = regImmB (>)
gtrr = regRegB (>)
eqir = immRegB (==)
eqri = regImmB (==)
eqrr = regRegB (==)

reg, imm :: (Int -> Int -> Int) -> [Int] -> [Int] -> Int
reg f xs [x,y] = (xs!!x) `f` (xs!!y)
imm f xs [x,y] = (xs!!x) `f` y

regRegB, regImmB, immRegB :: (Int -> Int -> Bool) -> [Int] -> [Int] -> Int
regRegB f xs [x,y] = case (xs!!x) `f` (xs!!y) of { True-> 1; False->0 }
regImmB f xs [x,y] = case (xs!!x) `f` y of { True-> 1; False->0 }
immRegB f xs [x,y] = case x `f` (xs!!y) of { True-> 1; False->0 }

functions :: [([Int] -> [Int] -> Int)]
functions = [addr
            , addi
            , multr
            , multi
            , banr
            , bani
            , borr
            , bori
            , setr
            , seti
            , gtir
            , gtri
            , gtrr
            , eqir
            , eqri
            , eqrr]

checkFun :: ([Int], [Int], [Int]) -> ([Int] -> [Int] -> Int) -> Bool
checkFun (xs, [num, a, b, c], res) f = (f xs [a,b]) == res!!c

solve1 :: String -> Int
solve1 = length . filter (==True) . map (\x-> length (filter (==True) $ map (checkFun x) functions) >= 3) . parseStr



functions2 :: [(String, ([Int] -> [Int] -> Int))]
functions2 = [("addr", addr)
            , ("addi", addi)
            , ("multr", multr)
            , ("multi", multi)
            , ("banr", banr)
            , ("bani", bani)
            , ("borr", borr)
            , ("bori", bori)
            , ("setr", setr)
            , ("seti", seti)
            , ("gtir", gtir)
            , ("gtri", gtri)
            , ("gtrr", gtrr)
            , ("eqir", eqir)
            , ("eqri", eqri)
            , ("eqrr", eqrr)]

checkFun2 :: ([Int], [Int], [Int]) -> (String, ([Int] -> [Int] -> Int)) -> Maybe (String, Int, ([Int] -> [Int] -> Int))
checkFun2 (xs, [num, a, b, c], res) x@(name, f) 
    | (f xs [a,b]) == res!!c = Just (name, num, f)
    | otherwise              = Nothing

getPossibleFuns :: ([Int], [Int], [Int]) -> [(String, Int, ([Int] -> [Int] -> Int))]
getPossibleFuns x = map fromJust . filter isJust . map (checkFun2 x) $ functions2

display :: (String, Int, ([Int] -> [Int] -> Int)) -> String
display = (\(x,_,_)->x)

display2 :: (String, ([Int] -> [Int] -> Int)) -> String
display2 = (\(x,_)->x)

step :: M.IntMap (String, ([Int] -> [Int] -> Int)) -> [[(String, Int, ([Int] -> [Int] -> Int))]] -> [[(String, Int, ([Int] -> [Int] -> Int))]] -> M.IntMap (String, ([Int] -> [Int] -> Int))
step fs [] [] = fs
step fs unsure [] = step (print3 $ fs) [] (print2 $ unsure)
step fs unsure ([(name, num, f)]:xs) = step (M.insert num (name, f) fs) unsure xs
step fs unsure ((x@(name, num, f):xs):ys) = case M.lookup num fs of
                                                Nothing -> step fs ((xs++[x]):unsure) ys
                                                Just a  -> step fs (xs:unsure) ys

print2 :: [[(String, Int, ([Int] -> [Int] -> Int))]] -> [[(String, Int, ([Int] -> [Int] -> Int))]]
print2 a = b where
    -- !c = unsafePerformIO . print . filter (not . null) . map (filter (\(_,x)->x == 1)) . map (map (\(x,y,_)-> (x,y))) $ a
    -- !c = unsafePerformIO . print . map (\xs-> (length xs, head xs)) . group . sort . concatMap (map length) . map (filter (not . null)) . map (\y-> map (\z-> filter (\(_,x)->x == z) y) [0..15]) . map (map (\(x,y,_)-> (x,y))) $ a
    -- !c = unsafePerformIO . print . map (map (\xs->(length xs, snd $ head xs))) . map (filter (not . null)) . map (\y-> map (\z-> filter (\(_,x)->x == z) y) [0..15]) . map (map (\(x,y,_)-> (x,y))) $ a
    !c = unsafePerformIO . print . nub . map (\xs->(length xs, snd $ head xs)) . filter (not . null) . concatMap (\y-> map (\z-> filter (\(_,x)->x == z) y) [0..15]) . map (map (\(x,y,_)-> (x,y))) $ a
    -- !c = unsafePerformIO . print . length $ a
    b = a

print3 :: M.IntMap (String, ([Int] -> [Int] -> Int)) -> M.IntMap (String, ([Int] -> [Int] -> Int))
print3 a = b where
    !c = unsafePerformIO . print . map (\(k, (x,_))-> (k, x)) . M.toList $ a
    b = a