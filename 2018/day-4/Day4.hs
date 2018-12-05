module Day4 where

import Data.Char
import Data.List
import Data.Time
import GHC.Integer
import GHC.Exts
import qualified Data.Map as Map

data Command = Guard Int | Sleeps | Wakes
    deriving(Eq, Show)
    
getGuard :: Command -> Int
getGuard (Guard x) = x
getGuard _         = error "WTF"

parseStr :: String -> [((Day, Int), Command)]
parseStr = map (\x-> ((time x), (toCommand x))) . sort . lines . filter (/='\r')
    
toCommand :: String -> Command
toCommand = fun . str where
    filterList = ["]", "Guard", "begins", "shift", "asleep", "up"]
    str = filter (not . flip elem filterList) . words . dropWhile (/=']')
    fun ["wakes"] = Wakes
    fun ["falls"] = Sleeps
    fun ['#':id] = Guard (read id)
    fun x = error $ show x

toMinutes :: DiffTime -> Int
toMinutes dt = fromIntegral . divInteger (diffTimeToPicoseconds dt) . fromIntegral $ (10 ^ 12 * 60)

time :: String -> (Day, Int)
time str = (utctDay t, toMinutes (utctDayTime t)) where
    t = parseTimeOrError True defaultTimeLocale "[%Y-%m-%d %H:%M" . takeWhile (/=']') $ str

sortMax :: [(Int, Int)] -> (Int, Int)
sortMax = head . reverse . sortWith snd





main :: IO()
main = do
        fileStr <- readFile "input.txt"
        print . solve1 $ fileStr
        print . solve2 $ fileStr


solve1 :: String -> Int
solve1 fileStr = maxId * maxHour where
    hours = machine (Map.fromList []) (0,0) .  parseStr $ fileStr
    maxId = fst . sortMax . addHours $ hours
    maxHour = fst . sortMax . snd . head . filter (\(x,_)-> x == maxId) $ hours



solve2 :: String -> Int
solve2 fileStr = fun maxHourOutOfAllGuards where
    hours = machine (Map.fromList []) (0,0) .  parseStr $ fileStr
    maxHourOutOfAllGuards = head . reverse . sortWith (snd . snd) . map (\(x, list)-> (x, sortMax list)) $ hours
    fun (id1, (min1,_)) = id1 * min1



addHours :: [(Int, [(Int, Int)])] -> [(Int, Int)]
addHours list = map (\(x,y)-> (x, fun y)) list where
    fun = foldl (\acc (_,x)-> acc + x) 0 



machine :: Map.Map Int [(Int, Int)] -> (Int, Int) -> [((Day, Int), Command)] -> [(Int, [(Int, Int)])]
machine kv _           []                          = Map.toList kv
machine kv (_, sId)    (((day,diff), Guard id):xs) = machine (Map.insertWith funInsert id timeList kv)        (0,id)      xs
    where
        funInsert tList []      = tList
        funInsert tList current = current
        timeList = zip [0..59] (repeat 0)
machine kv (_, sId)    (((day,diff), Sleeps):xs)   = machine kv                                              (diff, sId) xs
machine kv (sMin, sId) (((day,diff), Wakes):xs)    = machine (Map.insertWith fun sId [(sMin,0),(diff,0)] kv) (0,sId)     xs
    where
        fun ((sleep,_):(wake,_):[]) list = map (fun2 sleep wake) list
        fun2 :: Int -> Int -> (Int, Int) -> (Int, Int)
        fun2 sleep wake x@(t, n)
            | t >= sleep && t < wake = (t, n+1)
            | otherwise              = x
