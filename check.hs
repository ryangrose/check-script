import System.Environment (getArgs)
import Data.Time.LocalTime
import Data.List

interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (f input)

{- main = mainWith myFunction -}
  {- where mainWith function = do -}
          {- args <- getArgs -}
          {- case args of -}
            {- [input,output] -> interactWith function input output -}
            {- _ -> putStrLn "error: exactly two arguments needed" -}

        {- -- Replace id with actual function -}
        {- myFunction = id -}

appendCurrentTime = do
    currTime <- getZonedTime
    let time = takeWhile (/= '.') $ show currTime
    appendFile "out.txt" (time ++ "\n")

divideByDays [] = []
divideByDays (x:y:xs)
    | (day x) == (day y) = [x,y]: divideByDays xs
    | otherwise          = [x]: divideByDays (y:xs)

divideByDays (x:[]) = [[x]]

day d = takeWhile (/= ' ') d
time d = drop 1 $ dropWhile (/= ' ') d

showDayTimes dayTimes =
    d ++ ": " ++ (concat $ intersperse " " $ map time dayTimes)
    where d = day $ head dayTimes

printDays = do
    input <- readFile "out.txt"
    let inputLines = lines input
    let days = divideByDays inputLines
    let prettyDays = map showDayTimes days
    putStr $ unlines prettyDays

main = printDays
