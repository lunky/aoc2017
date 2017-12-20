module Day10
    ( 
    day10
   ,day10b
   ,twist
   ,twist'
   ,rotate
    )
    where

import qualified Data.Text as T
import Data.List


--window = [0..4]

day10 :: String -> [Int] -> Int
day10 input window = product $ take 2 $ twist 0 window $ map (\y -> read (T.unpack y)) $ T.splitOn (T.pack ",") (T.pack input)

twist :: Int -> [Int] -> [Int] -> [Int]
twist skipCount window counts = (\(window,pos,skip)-> rotate (length window -pos) window) $
                                    foldl' (\(currWindow,pos,skipCount) y  
                                        -> twist' (currWindow,pos,skipCount) y) (window,0,0) counts 
                                                               --  twist' skipSize input count

twist' :: ([a],Int,Int) -> Int -> ([a],Int,Int)
twist' (window,pos,skipCount ) count = (rotate skipCount $ (\y -> (take rest $ drop count y) ++ (reverse $ take count y) ) (cycle window),
                                        (pos + count + skipCount) `rem` length window, skipCount+1)
    where rest = (length window) - count 

rotate :: Int -> [a] -> [a]
rotate offset val = (take rest $ drop offset val) ++ take offset val
    where rest = length val -offset
    
day10b :: String -> Int
day10b input = 0
