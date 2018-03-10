import System.Environment (getArgs)
import Data.Time.LocalTime

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

printCurrentTime = do
    currTime <- getZonedTime
    let time = takeWhile (/= '.') $ show currTime
    appendFile "out.txt" (time ++ "\n")

main = printCurrentTime
