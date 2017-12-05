module Day5 
    ( 
    day5
   ,day5b
   ,move
    )
    where
day5 :: String -> Int
day5 input = move 0 0 $ map (\y -> (read y)::Int ) $ lines input

incAt :: Int -> [Int] -> [Int] -- increments element at index
incAt index ls = a ++ ((el+1):b) where (a, (el:b)) = splitAt index ls
decAt :: Int -> [Int] -> [Int] -- increments element at index
decAt index ls = a ++ ((el-1):b) where (a, (el:b)) = splitAt index ls

move :: Int -> Int -> [Int] -> Int
move count start list 
    | start > (length list - 1) = count
    | otherwise = move (count+1) (start + list!!start) (incAt start list)
        where nextStart = (start + list!!start)
        
moveB :: Int -> Int -> [Int] -> Int
moveB count start list 
    | start > (length list - 1) = count
    | otherwise = moveB (count+1) (start + list!!start) (operation start list)
        where nextStart = (start + list!!start)
              operation = if(list!!start>=3) then decAt else incAt

day5b :: String -> Int
day5b input = moveB 0 0 $ map (\y -> (read y)::Int ) $ lines input


-- current index
-- list 
-- 