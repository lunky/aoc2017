module Day15
    ( 
    day15
   ,day15b
   ,day15Function
   ,generator
    )
    where
    
import Text.Printf
import Data.Bits
import System.Directory
    

day15 :: Int -> String -> Int 
day15 count input = sum $ map last16test $ take count $ iterate generator $ parseInput input

day15b :: Int -> String -> Int
day15b count input = sum $ map last16test $ take count $ generatorB $ parseInput input

parseInput :: String -> (Int,Int)
parseInput input = (\[x,y] -> (x,y)) $ map (\y -> read $ head $ drop 4 $ words y ) $ lines input

constant = 2147483647
factorA= 16807
factorB = 48271

intToBinaryString :: Int -> String 
intToBinaryString arg = printf "%032b" arg

day15Function :: Int -> Int -> Int
day15Function factor input = input * factor `rem` constant

generator (a,b) = (day15Function factorA a, day15Function factorB b)

generatorB (a,b) = zip seriesA seriesB
    where seriesA = (filter (\y-> y `mod` 4 == 0) $ iterate (day15Function factorA) a)
          seriesB = (filter (\y -> y `mod` 8 ==0) $ iterate (day15Function factorB) b)

last16 :: (Bits a, Num a) => a -> a
last16 i =  ((.&.) i  (mask 16))
    where mask x =  (1 `shiftL` x) -1

last16test (a,b) 
    | last16 a == last16 b = 1
    | otherwise = 0

input = "Generator A starts with 65\nGenerator B starts with 8921"


