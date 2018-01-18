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
            | index+increment >= (length list) = 1 + (index+increment) `rem` (length list)
            | otherwise = 1 + index + increment
-- recursion 
advance2 increment (index,list) value= 
    (\(x,y) -> (x,insertAt (x) value y )) $  adv increment (index,list)
    where  adv counter (curr,list) 
                | counter == 0   = (curr+1, list )
                | curr == len = adv (counter-1) (0, list)
                | otherwise    = adv (counter-1) (next, list)
                where len = (length list) - 1
                      next = (curr+1)

insertAt :: Int -> Int-> [Int] -> [Int] 
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = splitAt z xs

day17b :: Int -> Int 
day17b increment = (\(_,list)->list!!1) $ foldl' advance' (0,[0]) [1..50000000]
    where advance' = advance increment
--day17 increment = (\(idx, list)-> list !! (idx+1)) $ foldl' advance' (0,[0]) [1..2017]