module Day3 
    ( 
    day3
   ,day3b
    )
    where
--    1	2 11 28 53 86
--    0 1 9 17 33

-- start at (0,0)
day3 :: Int -> Int
day3 input = (\(x,y)-> (abs x + abs y)-1) $ foldr(\x acc -> move x acc) (0,0) $ route input

circle = foldr (\(x,y) xs -> x:(y:xs) ) [] $ zip [1..] [1..]
set =  cycle ["right","up","left","down"]
instructions = zip circle (cycle set)

--travel :: String -> (Int,Int) -> Int -> (Int, Int)
--travel direction location 0 = ()
route :: Int -> [String]
route len = take len $ concat $ map (\(dir,y) -> map (\x -> dir) y ) $ zip set $ tupleToList $ (\a -> zip a a) $ map (\y -> (take y $ repeat 1) ) $ [x | x <- [1..]]
--route = zip set $ tupleToList $ (\a -> zip a a) $ map (\y -> (take y $ repeat 1) ) $ [x | x <- [1..]]

tupleToList :: [(a,a)] -> [a]
tupleToList ((a,b):xs) = a : b : tupleToList xs
tupleToList _ = []

move :: [Char] -> (Int, Int) -> (Int, Int)
move "right" (x,y) = (x+1,y) 
move "up"    (x,y) = (x,y+1)
move "left"  (x,y) = (x-1,y)
move _  (x,y) = (x,y-1)

    
day3b :: Int -> Int
day3b input = 0
