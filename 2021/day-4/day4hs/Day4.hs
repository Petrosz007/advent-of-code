module Day4 where

import Data.List (transpose, intercalate, find)
import Data.Text (pack, splitOn, unpack)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

data Cell = Cell
  { num :: Int,
    marked :: Bool
  }
  deriving (Show, Eq)

type Board = [[Cell]]


{--- Utilities ---}
split :: String -> String -> [String]
split separator str = map unpack $ splitOn (pack separator) (pack str)

matrixMap :: (a -> b) -> [[a]] -> [[b]]
matrixMap f = map (map f)

update :: (a -> Bool) -> (a -> a) -> [[a]] -> [[a]]
update pred f = matrixMap (\x -> if pred x then f x else x)


{--- Parsing ---}
toBoard :: [[Int]] -> Board
toBoard = matrixMap (\x -> Cell {num = x, marked = False})

boardStringToInts :: [String] -> [[Int]]
boardStringToInts = map (map (\x -> read x :: Int) . words)

parseInput :: String -> ([Int], [Board])
parseInput s = (nums, boards)
  where
    (numsRaw : _ : inputLines) = lines s
    nums = map read $ split "," numsRaw
    boardStrings = map (split "\n") $ split "\n\n" $ intercalate "\n" inputLines
    boards = map (toBoard . boardStringToInts) boardStrings


{--- Board Operations ---}
markNumber :: Int -> Board -> Board
markNumber target board = update ((==) target . num) (\cell -> cell {marked = True}) board

rows :: Board -> [[Cell]]
rows board = board

cols :: Board -> [[Cell]]
cols board = transpose board

cells :: Board -> [Cell]
cells = concat


{--- Bingo Operations ---}
hasBingo :: Board -> Bool
hasBingo board = rowBingo || colBingo
  where
    rowBingo = or . map (and . map marked) $ rows board
    colBingo = or . map (and . map marked) $ cols board

type PlayBingo = Int -> [Int] -> [Board] -> (Int, Board)

playBingoUntilFirstWin :: PlayBingo
playBingoUntilFirstWin prevNumber (n:ns) boards = case find hasBingo boards of
  Just board -> (prevNumber, board)
  Nothing    -> playBingoUntilFirstWin n ns (map (markNumber n) boards)

playBingoUntilLastWin :: PlayBingo
playBingoUntilLastWin prevNumber (n:ns) [board] = if hasBingo board
  then (prevNumber, board)
  else playBingoUntilLastWin n ns [(markNumber n board)]
playBingoUntilLastWin prevNumber (n:ns) boards = playBingoUntilLastWin n ns . map (markNumber n) . filter (not . hasBingo) $ boards


{--- Solutions ---}
solve :: PlayBingo -> [Int] -> [Board] -> Int
solve playBingo nums boards = lastNumber * sumOfUnmarked
  where
    (lastNumber, bingoBoard) = playBingo 0 nums boards
    sumOfUnmarked = sum . map num . filter (not . marked) . cells $ bingoBoard

solve1 :: [Int] -> [Board] -> Int
solve1 = solve playBingoUntilFirstWin

solve2 :: [Int] -> [Board] -> Int
solve2 = solve playBingoUntilLastWin

main :: IO ()
main = do
  (nums, boards) <- parseInput <$> readFile "input.txt"
  let part1 =  solve1 nums boards
  let part2 =  solve2 nums boards
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2
