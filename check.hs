import System.Environment (getArgs)
import Data.Time.LocalTime
import Data.List
import Text.Show.Functions()


type Day = String
type Time = String

data Date = Date Day Time
    deriving (Show, Read, Eq, Ord)

zonedTimeToDate :: (Show t) =>  t -> Date
zonedTimeToDate z = Date day time
    where day = takeWhile (/= ' ') $ show z
          time = takeWhile (/= '.') $ tail $ dropWhile (/= ' ') $ show z

recordTime :: FilePath -> IO ()
recordTime filename = do 
    t <- getZonedTime
    appendFile filename . show . zonedTimeToDate $ t
    appendFile filename "\n"

parseDates :: [String] -> [Date]
parseDates = map read 

getDates :: FilePath -> IO [Date]
getDates inputFile = do
    total <- readFile inputFile
    return $ (parseDates . lines) total

groupByDays :: [Date] -> [[Date]]
groupByDays = groupBy (\(Date day _) (Date otherday _) -> day == otherday)

showGroupedDays :: [Date] -> String
showGroupedDays [] = ""
showGroupedDays days = day ++ ":\n  " ++ concat times
    where times = intersperse "\n  " $ map (\(Date _ t) -> t) days
          day   = (\(Date d _) -> d) $ head days 

showDays :: [Date] -> [String]
showDays = map showGroupedDays . groupByDays . sort

printDays :: FilePath -> IO ()
printDays filename =  do
    dates <- getDates filename
    putStrLn . unlines $ showDays dates

main :: IO ()
main = do
    args <- getArgs
    if null args then 
        printDays "out.txt"
    else
        recordTime "out.txt" 
