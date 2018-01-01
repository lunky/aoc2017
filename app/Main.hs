module Main where

import Day1
import Day2
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14

import Data.List
import Data.Function

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main :: IO ()
main =  do  
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
    
--    contents <- readFile "input/5.txt"
--    let answer = show $ day5  contents
--    putStrLn ("day5: " ++ answer)
    putStrLn ("day5: " ++ "too slow to run")
--    let answer = show $ day5b  contents
--    putStrLn ("day5b: " ++ answer)
    putStrLn ("day5b: " ++ "too slow to run")
    

    contents <- readFile "input/6.txt"
    let answer = show $ day6 contents
    putStrLn ("day6: " ++ answer)
    let answer = show $ day6b contents
    putStrLn ("day6b: " ++ answer)
    
    contents <- readFile "input/7.txt"
    let answer = show $ day7 contents
    putStrLn ("day7: " ++ answer)
    let answer = show $ day7b contents
    putStrLn ("day7b: " ++ answer)
    
    contents <- readFile "input/8.txt"
    let answer = show $ day8 contents
    putStrLn ("day8: " ++ answer)
    let answer = show $ day8b contents
    putStrLn ("day8b: " ++ answer)
    
    contents <- readFile "input/9.txt"
    let answer = show $ day9 contents
    putStrLn ("day9: " ++ answer)
    let answer = show $ day9b contents
    putStrLn ("day9b: " ++ answer)
    
    contents <- readFile "input/10.txt"
    let answer = show $ day10 contents [0..255]
    putStrLn ("day10: " ++ answer)
    let answer = show $ day10b contents [0..255]
    putStrLn ("day10b: " ++ answer)
    
    contents <- readFile "input/11.txt"
    let answer = show $ day11 contents 
    putStrLn ("day11: " ++ answer)
    let answer = show $ day11b contents
    putStrLn ("day11b: " ++ answer)

    contents <- readFile "input/12.txt"
    let answer = show $ day12 contents 
    putStrLn ("day12: " ++ answer)
    let answer = show $ day12b contents
    putStrLn ("day12b: " ++ answer)
    
    contents <- readFile "input/13.txt"
    let answer = show $ day13 contents 
    putStrLn ("day13: " ++ answer)
    --let answer = show $ day13b contents
    --putStrLn ("day13b: " ++ answer)
    putStrLn ("day13b: " ++ "too slow to run")
    
    let contents = "jxqlasbh"
    let answer = show $ day14 contents 
    putStrLn ("day14: " ++ answer)
    let answer = show $ day14b contents
    putStrLn ("day14b: " ++ answer)