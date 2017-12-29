module Day13
    ( 
    day13
   ,day13b
    )
    where
import Text.ParserCombinators.ReadP
import Data.List (find)
import Data.Maybe (catMaybes)

day13 :: String -> Int
day13 input = sum $ map (\(x,y,_)-> x*y) $ filter(\(_,_,y) -> y==1) $ catMaybes $ (\y -> y) $ map (\(lst, index) -> find (\(y,_,_) -> y==index ) lst) $ zip (day13' input) [0..]
day13' input = map (\y -> runFirewall y layers) [0..layerCount] 
    where   layers = parseInput input
            layerCount = maximum $ map fst layers

--[runFirewall :: Int -> [(Int,Int)] ->  [(Int,Int,Int)]
runFirewall picoSecond layers = map (\layer -> setLayer picoSecond layer) layers

setLayer picoSecond (layer,range) =  (layer, range, (oscillate range) !! (picoSecond))

parseInput = concatMap (fmap fst . filter ((== "") . snd)) . fmap parseLine . lines 

--score (layer, range, scanner) curr


input = "0: 3\n1: 2\n4: 4\n6: 4"

oscillate range  
    | range <=0 = []
    | range ==1 = cycle [1]
    | otherwise = cycle [x | x <- [1..range]++[range-1,range-2..2]]

parseNumber :: ReadP Int
parseNumber = read <$> munch (`elem` ['0'..'9'])

parseLine :: ReadS (Int, Int)
parseLine =
  readP_to_S
  $ pure (\x xs -> (x, xs))
  <*> parseNumber
  <*  skipSpaces
  <*  string ":"
  <*  skipSpaces
  <*> parseNumber
  <*  eof


day13b :: String -> Int
day13b input = 0

