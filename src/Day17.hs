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
day17b increment = snd $ foldl' advance' (0,0) [1..50000000]
    where advance' = advanceB increment
-- this version doesn't even track a list, that was using all the memory on my laptop
-- even though this version doesn't use a list it still uses a tonne of ram for some reason
-- I don't know why. TODO -- fix RAM usage
-- maybe a pure recursive version?
advanceB increment (index,element1) value = (next,if(next==1) then (value) else element1)
    where 
          next 
            | indexNIncrement >= value = 1 + (indexNIncrement) `rem` (value)
            | otherwise = 1 + indexNIncrement
          indexNIncrement = index + increment
