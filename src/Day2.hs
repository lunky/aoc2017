module Day2 
    ( 
    day2
   ,day2b
    )
    where
import Data.List
day2 input =  sum $ map (\y -> maximum y - minimum y  ) $ cleanLine 
        where  cleanLine = map (map (\z -> (read z)::Integer)) $ map words $ lines input
    
day2b :: String -> Int
day2b input = sum $ map division $ map (\y -> head $ match y)  $  map (\y -> combos y) cleanLine
        where  cleanLine = map (map (\z -> (read z)::Integer)) $ map words $ lines input
               match = filter (\(a,b) -> (maximum([a,b]) `mod` minimum([a,b])) == 0 )
               combos line = [ (x,y) | (x:rest) <- tails (line) , y <- rest ] 
               division (a,b) = fromIntegral( maximum [a,b] `div` minimum([a,b]))