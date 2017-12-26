module Day11
    ( 
    day11
   ,day11b
   ,move
   ,diag
    )
    where

import Data.List (foldl')
import qualified Data.Text as T


day11 :: String -> Int
day11 input = diag (0,0) $ move (0,0) input

day11b :: String -> Int
day11b input = maximum $ map (\y -> diag (0,0) y) $ scanmove (0,0) input

move :: (Int,Int) -> String -> (Int,Int)
move location input = foldr move' location $ map (\y -> T.unpack y) $ T.splitOn (T.pack ",") (T.pack input)

scanmove :: (Int,Int) -> String -> [(Int,Int)]
scanmove location input = scanl (\acc y -> move' y acc) location $ map (\y -> T.unpack y) $ T.splitOn (T.pack ",") (T.pack input)

move' :: String -> (Int,Int) -> (Int,Int)
move' "n"  (x,y) = (x,y+1)
move' "s"  (x,y) = (x,y-1)

move' "ne" (x,y) = (x+1,y)
move' "sw" (x,y) = (x-1,y)

move' "se" (x,y) = (x+1,y-1)
move' "nw" (x,y) = (x-1,y+1)

diag:: (Int,Int) -> (Int,Int) -> Int
diag (x1,y1) (x2,y2)  = maximum [x2-x1, y2-y1, z2-z1]
    where (z1,z2) = ((x1+y1)*(-1), (x2+y2)*(-1))