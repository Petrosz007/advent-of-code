module Day4 where 
    
import Data.Time
import Data.List (groupBy, isInfixOf, deleteBy)
import GHC.Integer(divInteger)
import GHC.Exts(sortWith, groupWith)

main :: IO()
main = do
  fileStr <- readFile "input.txt"
  let guards = groupGuards . parseInput $ fileStr
  print . apply (fst, bestMinute . map (map toMinutes) . selectDiffTimes) . last . sortWith totalSleep $ guards
  let ((m, _), (idx, _)) = last . sortWith (snd . fst) . zip (map (bestMinute' . map (map toMinutes) . selectDiffTimes) guards)  $ guards
  print $ m * idx

toMinutes :: DiffTime -> Int
toMinutes dt = fromIntegral $ divInteger (diffTimeToPicoseconds dt) $ fromIntegral (10 ^ 12 * 60)

time :: String -> UTCTime
time = parseTimeOrError True defaultTimeLocale "[%Y-%m-%d %H:%M" . takeWhile (/=']')

data Action = Begins Int | FallsASleep | WakesUp
  deriving (Eq, Show)

apply :: (a -> b, a -> c) -> a -> (b, c)
apply (f, g) a = (f a, g a)

getGuardId :: String -> Int
getGuardId = read . takeWhile (/=' ') . drop 1 . dropWhile (/='#')

toAction :: String -> Action
toAction s
  | isInfixOf "falls asleep" s = FallsASleep
  | isInfixOf "wakes up" s     = WakesUp
  | otherwise                  = Begins $ getGuardId s

getId :: Action -> Int
getId (Begins i) = i

isId :: Action -> Bool
isId (Begins _) = True
isId _          = False

readLine :: String -> (UTCTime, Action)
readLine = apply (time, toAction)

ignore :: (a -> b) -> c -> a -> b
ignore f _ = f

extractId :: [(UTCTime, Action)] -> (Int, [(UTCTime, Action)])
extractId = apply (getId . snd . head . filter (isId . snd), id)

concatGuard :: [(Int, [(UTCTime, Action)])] -> (Int, [(UTCTime, Action)])
concatGuard = apply (fst . head, concatMap snd)

groupGuards :: [(UTCTime, Action)] -> [(Int, [(UTCTime, Action)])]
groupGuards = map concatGuard . groupWith fst . sortWith fst . map extractId . groupBy (ignore $ not . isId . snd) . sortWith fst

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go where
  go t = case splitAt k t of
    (a,b) | null a    -> []
          | otherwise -> a : go b

selectDiffTimes :: (Int, [(UTCTime, Action)]) -> [[DiffTime]]
selectDiffTimes = chunksOf 2 . map (utctDayTime . fst) . filter (not . isId . snd) . snd

contains :: Int -> [Int] -> Int
contains i (a:b:[]) | i >= a && i < b = 1
                    | otherwise       = 0

bestMinute :: [[Int]] -> Int
bestMinute = fst . bestMinute'

maxIndex :: (Int, Int) -> (Int, Int) -> (Int, Int) 
maxIndex (ix, i) (ix2, m) = if i > m then (i, ix) else (m, ix2)

bestMinute' :: [[Int]] -> (Int, Int)
bestMinute' xs = head
               $ sortWith fst
               $  (\ys -> filter ((==) (snd $ last ys) . snd) ys)
               $ sortWith snd
               $ map (apply (id, sum . flip map xs . contains)) [0..59]

timeDiff :: [[DiffTime]] -> DiffTime
timeDiff = sum . map (\(a:b:[]) -> b - a)

totalSleep :: (Int, [(UTCTime, Action)]) -> DiffTime
totalSleep =  timeDiff . selectDiffTimes

parseInput :: String -> [(UTCTime, Action)]
parseInput = map readLine . lines . filter (/='\r')