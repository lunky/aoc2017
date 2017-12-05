module Day5 
    ( 
    day5
   ,day5b
   ,move
    )
    where
import Data.Vector ((!), (//))
import qualified Data.Vector as V

day5 :: String -> Int
day5 input = move 0 0 $ V.fromList $ map (\y -> (read y)::Int ) $ lines input

incAt :: Int -> V.Vector Int -> V.Vector Int -- increments element at index
incAt index ls = ls V.// [(index, (ls ! index)+1)]
decAt :: Int -> V.Vector Int -> V.Vector Int -- increments element at index
decAt index ls = ls // [(index,(ls ! index)-1)]
    
--move :: Int -> Int -> Vector Int -> Int
move :: Int -> Int -> V.Vector Int -> Int
move count start list 
    | start > (length list - 1) = count
    | otherwise = move (count+1) (start + list ! start) (incAt start list)
        where nextStart = (start + list ! start)
        
moveB :: Int -> Int -> V.Vector Int -> Int
moveB count start list 
    | start > (length list - 1) = count
    | otherwise = moveB (count+1) (start + list ! start) (operation start list)
        where nextStart = (start + list ! start)
              operation = if(list ! start >=3) then decAt else incAt

day5b :: String -> Int
day5b input = moveB 0 0 $ V.fromList $ map (\y -> (read y)::Int ) $ lines input


-- current index
-- list 
-- 