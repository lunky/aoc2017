module Day17
    ( 
    day17
   ,day17b
   ,advance
    )
    where
    
import Data.List (foldl',findIndex)
import Data.Maybe (fromJust)
    
day17 :: Int -> Int 
day17 increment = (\(idx, list)-> list !! (idx+1)) $ foldl' advance' (0,[0]) [1..2017]
    where advance' = advance increment

advance increment (index,list) value = (next,insertAt (next) value list)
    where 
          next 
            | indexNIncrement >= (value) = 1 + (indexNIncrement) `rem` (value)
            | otherwise = 1 + indexNIncrement
          indexNIncrement = index + increment
            
insertAt :: Int -> Int-> [Int] -> [Int] 
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = Prelude.splitAt z xs

day17b :: Int -> Int 
day17b increment = (\(_,list)->list !!  1) $ foldl' advance' (0,[0]) [50000000..50000001]
    where advance' = advance increment
--day17 increment = (\(idx, list)-> list !! (idx+1)) $ foldl' advance' (0,[0]) [1..2017]
