module Day4 where

import Data.List (groupBy, sortBy, intercalate, find)
import Data.Text (pack, splitOn, unpack)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

data Pos = Pos
  { row :: Int,
    col :: Int
  }
  deriving (Show, Eq)

data Cell = Cell
  { pos :: Pos,
    num :: Int,
    marked :: Bool
  }
  deriving (Show, Eq)

type Board = [Cell]


{--- Utilities ---}
split :: String -> String -> [String]
split separator str = map unpack $ splitOn (pack separator) (pack str)

update :: (a -> Bool) -> (a -> a) -> [a] -> [a]
update pred f = map (\x -> if pred x then f x else x)


{--- Parsing ---}
toBoard :: [[Int]] -> Board
toBoard =
  concatMap (\(row, xs) -> map (\(col, val) -> Cell {pos = Pos {row = row, col = col}, num = val, marked = False}) xs)
    . map (\(row, line) -> (row, zip [0 ..] line))
    . zip [0 ..]

boardStringToInts :: [String] -> [[Int]]
boardStringToInts = map (map (\x -> read x :: Int) . words)

parseInput :: String -> ([Int], [Board])
parseInput s = (nums, boards)
  where
    (numsRaw : _ : inputLines) = lines s
    nums = map read $ split "," numsRaw
    boardStrings = map (split "\n") $ split "\n\n" $ intercalate "\n" inputLines
    boards = map (toBoard . boardStringToInts) boardStrings


{--- Board Manipulation ---}
markNumber :: Int -> Board -> Board
markNumber target board = update ((==) target . num) (\cell -> cell {marked = True}) board

cellsEqBy :: Eq a => (Cell -> a) -> Cell -> Cell -> Bool
cellsEqBy lens x y = (lens x) == (lens y)

cellsCompareBy :: Ord a => (Cell -> a) -> Cell -> Cell -> Ordering
cellsCompareBy lens x y = compare (lens x) (lens y)

rows :: Board -> [[Cell]]
rows board = groupBy (cellsEqBy (row . pos)) $ sortBy (cellsCompareBy (row . pos)) board

cols :: Board -> [[Cell]]
cols board = groupBy (cellsEqBy (col . pos)) $ sortBy (cellsCompareBy (col . pos)) board


{--- Bingo Operations ---}
hasBingo :: Board -> Bool
hasBingo board = rowBingo || colBingo
  where
    rowBingo = or . map (and . map marked) $ rows board
    colBingo = or . map (and . map marked) $ cols board

playBingoUntilFirstWin :: Int -> [Int] -> [Board] -> (Int, Board)
playBingoUntilFirstWin prevNumber (n:ns) boards = case find hasBingo boards of
  Just board -> (prevNumber, board)
  Nothing    -> playBingoUntilFirstWin n ns (map (markNumber n) boards)

playBingoUntilLastWin :: Int -> [Int] -> [Board] -> (Int, Board)
playBingoUntilLastWin prevNumber (n:ns) [board] = if hasBingo board
  then (prevNumber, board)
  else playBingoUntilLastWin n ns [(markNumber n board)]
playBingoUntilLastWin prevNumber (n:ns) boards = playBingoUntilLastWin n ns . map (markNumber n) . filter (not . hasBingo) $ boards


{--- Solutions ---}
solve :: (Int -> [Int] -> [Board] -> (Int, Board)) -> [Int] -> [Board] -> Int
solve playBingo nums boards = lastNumber * sumOfUnmarked
  where
    (lastNumber, bingoBoard) = playBingo 0 nums boards
    sumOfUnmarked = sum . map num . filter (not . marked) $ bingoBoard

solve1 :: [Int] -> [Board] -> Int
solve1 = solve playBingoUntilFirstWin

solve2 :: [Int] -> [Board] -> Int
solve2 = solve playBingoUntilLastWin

main = do
  (nums, boards) <- parseInput <$> readFile "input.txt"
  let part1 =  solve1 nums boards
  let part2 =  solve2 nums boards
  putStrLn $ "Part 1: " ++ show part1
  putStrLn $ "Part 2: " ++ show part2
