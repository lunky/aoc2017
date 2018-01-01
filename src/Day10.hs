module Day10
    ( 
    day10
   ,day10b
   ,twist'
   ,runTwists
   ,parseInput
    )
    where

import qualified Data.Text as T (splitOn, pack, unpack)
import Data.List (foldl')
import Data.Char (ord)
import Data.Bits (xor)

parseInput :: String -> [Int]
parseInput input = map (\y -> read (T.unpack y)) $ T.splitOn (T.pack ",") (T.pack input)

day10 :: String -> [Int] -> Int
day10 input window = product $ take 2 $ runTwists window 1 $ parseInput input

day10b :: String -> [Int] -> String
day10b input window = concatMap hex $ map(\x -> (foldr1 (\y acc-> y `xor` acc) ) x) $ 
                chunks 16 $ 
                runTwists window 64 $ 
                (map ord input) ++ [17, 31, 73, 47, 23]
                
runTwists window iterations counts =  (\(wind,pos,skip)-> (rotate (length wind-pos) wind)) $
                                      runTwists' window iterations counts
runTwists' window iterations counts = foldr (\y acc -> twist acc counts) (window,0,0) [1..iterations] 

twist ::([Int],Int,Int) -> [Int] -> ([Int],Int,Int)
twist (window,inPos,inSkip) counts = foldl twist' (window,inPos,inSkip) counts 

twist' :: ([a],Int,Int) -> Int -> ([a],Int,Int)
twist' (window,pos,skipCount ) count = 
            (rotate skipCount $ (\y -> (take rest $ drop count y) ++ (reverse $ take count y) ) (cycle window),
                                        (pos + count + skipCount) `rem` length window, skipCount+1)
    where rest = (length window) - count 
          count' = count 

rotate :: Int -> [a] -> [a]
rotate offset val = take (length val) $ drop offset (cycle val) 

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

hex :: Int -> String
hex num = [digits !! div num 16,digits !! mod num 16]
    where digits = "0123456789abcdef"

input = "3,4,1,5"