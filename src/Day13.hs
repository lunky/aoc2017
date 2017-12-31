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
    where  day13' :: Int -> String -> [[(Int,Int,Int)]]                
           day13' start input = map (\y -> runPicoSecond y layers) [start..start+layerCount] 
           layers = parseInput input
           layerCount = maximum $ map fst layers
           runPicoSecond :: Int -> [(Int,Int)] ->  [(Int,Int,Int)]
           runPicoSecond picoSecond layers = map (\layer -> setLayer picoSecond layer) layers
            
day13b :: String -> Int
day13b input = snd $head $ dropWhile (\(y,_) -> y/=True) $ map (\y -> (tick y input,y)) [1..]

tick :: Int -> String -> Bool
tick picoSecond input = all (==False) $ map (\(layer,range) -> caught (picoSecond+layer) range ) $ parseInput input

caught :: Int -> Int -> Bool
caught picoSecond range =   (oscillateIndex range picoSecond) == 1

setLayer :: Int -> (Int, Int) -> (Int,Int,Int)
setLayer picoSecond (layer,range) =  (layer, range, (oscillate range) !! (picoSecond))

input = "0: 3\n1: 2\n4: 4\n6: 4"

oscillate :: Int -> [Int]
oscillate range  
    | range <=0 = []
    | range ==1 = cycle [1]
    | otherwise = cycle [x | x <- [1..range]++[range-1,range-2..2]]

oscillateIndex :: Int -> Int -> Int
oscillateIndex range index = (oscillate range) !! (index - ((index `div` range') * range'))
    where range' = (range-1)*2

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
