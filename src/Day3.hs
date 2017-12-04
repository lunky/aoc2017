module Day3 
    ( 
    day3
   ,day3b
   ,route
    )
    where
import Data.List

-- start at (0,0)
day3 :: Int -> Int
day3 input = (\(x,y)-> (abs x + abs y)-1) $ 
                foldr(\x acc -> move x acc) (0,0) $ take input $ route

day3b :: Int -> Int
day3b input = head $  
                map snd $ 
                foldl' (\acc x-> (rank x acc) : acc ) [] $ 
                scanl'(\acc x -> move x acc) (0,0) $ 
                take (input -1) route


route :: [String]
route = concat $ 
        map (\(dir,y) -> map (\x -> dir) y ) $ 
        zip set $ tupleToList $ (\a -> zip a a) $ 
        map (\y -> (take y $ repeat 1) ) [1..]
    where set =  cycle ["right","up","left","down"]

adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = map (\(a,b) -> (a+x,b+y))  [(x,y)| x <- [-1..1], y<-[-1..1]]

rank :: (Int,Int) -> [((Int,Int),Int)] -> ((Int,Int),Int)
rank (0,0) _ = ((0,0),1)
rank pos prev = (\y -> (pos,y)) $ 
                sum $ map snd $ 
                intersectBy (\(pos1,rank1) (pos2, rank2) -> pos1==pos2)  
                prev (map (\y->(y,99)) $ adjacent pos)

tupleToList :: [(a,a)] -> [a]
tupleToList ((a,b):xs) = a : b : tupleToList xs
tupleToList _ = []

move :: [Char] -> (Int, Int) -> (Int, Int)
move "right" (x,y) = (x+1,y) 
move "up"    (x,y) = (x,y+1)
move "left"  (x,y) = (x-1,y)
move "down"  (x,y) = (x,y-1)

    


