{-# LANGUAGE BangPatterns #-}
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
insertAt :: Int -> Int-> [Int] -> [Int] 
insertAt z y xs = as ++ (y:bs)
                  where (as,bs) = Prelude.splitAt z xs

advance increment (index,list) value = (next,insertAt (next) value list)
    where 
          next 
            | indexNIncrement >= (value) = 1 + (indexNIncrement) `rem` (value)
            | otherwise = 1 + indexNIncrement
          indexNIncrement = index + increment
            

day17b :: Int -> Int -> Int
day17b increment spins = snd $ foldl' advance' (0,0) [1..spins]
    where advance' = spin increment
-- this version doesn't even track a list, just the value in the 2nd column
spin increment (!index,!element1) spinNumber = (next,nextElementOne)
  where
    next 
      | indexNIncrement >= spinNumber = 1 + (indexNIncrement `rem` spinNumber)
      | otherwise = 1 + indexNIncrement
    indexNIncrement = index + increment
    nextElementOne
      | next==1 = spinNumber
      | otherwise = element1
          