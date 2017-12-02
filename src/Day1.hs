module Day1 
    ( 
    day1
   ,day1b
    )
    where
import Data.Char (digitToInt)

day1 :: String -> Int
day1 input = sum $ map (digitToInt.fst) $ filter (\(x,y)-> x==y) $ zip a (tail a ++ [head a])
    where a = input
day1b :: String -> Int
day1b input = sum $ map (digitToInt.fst) $ filter (\(x,y)-> x==y) $ zip input a
    where a = (drop half input) ++ (take (half+1) input)
          half = length input `div` 2
    
