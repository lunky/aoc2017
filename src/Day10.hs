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
import Data.Char 
import Data.Ord
import Data.Bits (xor)


day10 :: String -> [Int] -> Int
day10 input window = product $ take 2 $ twist window $ map (\y -> read (T.unpack y)) $ T.splitOn (T.pack ",") (T.pack input)

twist ::[Int] -> [Int] -> [Int]
twist window counts = (\(wind,pos,skip)-> rotate (length window -pos) wind) $
                                    foldl (\(currWindow,pos,skipCount) y  
                                        -> twist' (currWindow,pos,skipCount) y) (window,0,0) counts 
                                        
twistb ::([Int],Int,Int) -> [Int] -> ([Int],Int,Int)
twistb (window,inPos,inSkip) counts = (\(wind,pos,skip)-> (rotate (length window -pos) wind,(pos),skip)) $
                                    foldr (\y (currWindow,pos,skipCount)   
                                        -> twist' (currWindow,pos,skipCount) y) (window,inPos,inSkip) counts 
                                        
twist' :: ([a],Int,Int) -> Int -> ([a],Int,Int)
twist' (window,pos,skipCount ) count = (rotate skipCount $ (\y -> (take rest $ drop count y) ++ (reverse $ take count y) ) (cycle window),
                                        (pos + count + skipCount) `rem` length window, skipCount+1)
    where rest = (length window) - count 
          count' = count 

rotate :: Int -> [a] -> [a]
rotate offset val = (take rest $ drop offset val) ++ take offset val
    where rest = length val -offset
    
--day10b :: String -> [a] -> String
day10b input window = concatMap hex $ map(\x -> (foldl' (\y acc-> y ^ acc) 0) x) $ 
                chunks 16 $ 
                runTwists window 64 $ 
                (map ord input) ++ [17, 31, 73, 47, 23]
                where (^) = xor
                
runTwists window iterations counts = (\(result,_,_)->result) $ runTwists' window iterations counts

runTwists' window iterations counts = foldl' (\acc y -> twistb acc counts) (window,0,0) [1..iterations] 

day10b'  window iterations counts  = concatMap hex $ map(\x -> (foldl (\y acc -> y `xor` acc) 0) x) $ chunks 16 $ runTwists window iterations counts

test = 0

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs


hex :: Int -> String
hex num = [digits !! div num 16,digits !! mod num 16]
    where digits = "0123456789abcdef"
         