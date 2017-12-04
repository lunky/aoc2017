module Day4 
    ( 
    day4'
   ,day4
   ,day4b
   ,day4b'
    )
    where
import Data.List

day4' :: String -> Bool
day4' input = (length $ words input) == (length $ nub $ words input)

day4 :: String -> Int
day4 input = length $ filter (\y -> day4' y) $ lines input

day4b' :: String -> Bool
day4b' input = (length $ map sort $ words input) == (length $ nub $ map sort $ words input)

day4b :: String -> Int
day4b input = length $ filter (\y -> day4b' y) $ lines input
