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
day13 input = sum $ map (\(x,y,_)-> x*y) 
                $ filter(\(_,_,y) -> y==1) 
                $ catMaybes
                $ map (\(lst, index) -> find (\(y,_,_) -> y==index ) lst) 
                $ zip (day13' 0 input) [0..]
                
day13' start input = map (\y -> runPicoSecond y layers) [start..start+layerCount] 
    where   layers = parseInput input
            layerCount = maximum $ map fst layers
            
--day13b' :: Int -> String -> Int
day13b' start input = length
                $ filter(\(_,_,y) -> y==1) 
                $ catMaybes
                $ map (\(lst, index) -> find (\(y,_,_) -> y==index ) lst) 
                $ zip (day13' start input) [0..]
                
day13b :: String -> Int
day13b input = snd $head $ dropWhile (\y -> (fst y)/=0) $  zip ( map (\y -> day13b' y input) [1..]) [1..] 

runPicoSecond :: Int -> [(Int,Int)] ->  [(Int,Int,Int)]
runPicoSecond picoSecond layers = map (\layer -> setLayer picoSecond layer) layers

setLayer :: Int -> (Int, Int) -> (Int,Int,Int)
setLayer picoSecond (layer,range) =  (layer, range, (oscillate range) !! (picoSecond))

input = "0: 3\n1: 2\n4: 4\n6: 4"

oscillate :: Int -> [Int]
oscillate range  
    | range <=0 = []
    | range ==1 = cycle [1]
    | otherwise = cycle [x | x <- [1..range]++[range-1,range-2..2]]

parseInput :: String -> [(Int,Int)]
parseInput = concatMap (fmap fst . filter ((== "") . snd)) . fmap parseLine . lines 
    where parseNumber :: ReadP Int
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



