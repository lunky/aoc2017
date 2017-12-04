module Lib
    ( someFunc
    ) where
    

import Day1
import Day2
import Day4

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

someFunc :: IO ()
someFunc =  do  
    contents <- readFile "input/1.txt"
    let answer = show $ day1  (head $ lines $ contents)
    putStrLn ("day1: " ++ answer)
    let answer = show $ day1b (head $ lines $ contents)
    putStrLn ("day1b: " ++ answer)
    putStrLn "+++++++++++++++++++++++++++++"
    contents <- readFile "input/2.txt"
    let answer = show $ day2  contents
    putStrLn ("day2: " ++ answer)
    let answer = show $ day2b contents
    putStrLn ("day2b: " ++ answer)
    
    contents <- readFile "input/4.txt"
    let answer = show $ day4  contents
    putStrLn ("day4: " ++ answer)
    let answer = show $ day4b contents
    putStrLn ("day4b: " ++ answer)
    
