module Day6 
    ( 
    day6
   ,day6b
   ,redistribute 
   ,scatter
    )
    where
import Data.Maybe
import Data.List
import Data.Set (member)
import qualified Data.Set as S

day6 :: String -> Int
day6 input = (\y -> (length y)) $ tail $ takeUntilDuplicate $  iterate redistribute $ map (\y -> (read y)::Int ) $ words input

takeUntilDuplicate :: Ord a => [a] -> [a]
takeUntilDuplicate xs = foldr go (const []) xs S.empty
  where
    go x cont set
      | x `member` set = [x]
      | otherwise      = x : cont (S.insert x set)


redistribute :: [Int] -> [Int] 
redistribute set = disperse maxVal set 
    where (maxVal,maxIdx) = (maximum set, 1 + (fromJust $ elemIndex (maximum set) set) )
          len :: Int
          len = length set
          disperse val elements  =  
                (\y -> drop (len - maxIdx) y ++ (take (len - maxIdx) y )) $ 
                --snd $ foldr (\y (acc,lst) -> if acc>0 then (acc-1,y+1:lst) else (acc,y:lst)) (val,[])
                (\y -> scatter val y)
                $ drop maxIdx elements ++ (take (maxIdx -1) elements)++[0]
scatter theVal theSet =  snd $ head $ dropWhile(\(x,_)->x>0) $ iterate ((\(amount,curr) -> ( foldl' (\(acc,lst) y-> if acc>0 then (acc-1,lst ++ [y+1]) else (0,lst++[y])) (amount,[]) (curr)) )) (theVal, theSet)
--scatter theVal theSet =  snd $ head $ dropWhile(\(x,_)->x>0) $ iterate ((\(amount,curr) -> ( foldl' (\(acc,lst) y-> if acc>0 then (acc-1,y+1:lst) else (0,y:lst)) (amount,[]) (curr)) )) (theVal, theSet)
--take 1 $ dropWhile (\(x,_)->x>0) $ iterate  (\(amount,curr) -> ( foldl (\(acc,lst) y -> if acc>=0 then (acc-1,y+1:lst) else (acc,y:lst)) (amount,[]) curr) ) (7, [0,0,2,0])
day6b :: String -> Int
day6b input = (\(x:xs)-> fromJust(elemIndex x xs)+1)$ reverse $ takeUntilDuplicate $  iterate redistribute $ map (\y -> (read y)::Int ) $ words input


--let scatter val theSet =  (\(amount,curr) -> ( foldl' (\(acc,lst) y-> if acc>0 then (acc-1,y+1:lst) else (0,y:lst)) (amount,[]) (curr)) ) (val, theSet)