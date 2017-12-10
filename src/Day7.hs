{-# LANGUAGE OverloadedStrings #-}
module Day7 
    ( 
    day7
   ,day7b
   ,parseTreeString
   ,findBottom
   ,findChildren
   ,locateNode
   ,input
    )
    where
import Data.String (fromString)
import Data.List (find)
import Data.Maybe

day7 :: String -> String
day7 input = findBottom $ locateNode (findHead $ parseTreeString input) $ parseTreeString input
               
--day7 input = 0



parseTreeString input = map (\(program:details) -> (program, (\y->(read y)::Int) $ 
    init $ drop 1 $ head details, parseDetails $ tail details) ) $
    map words $ lines input
    where parseDetails []=[]
          parseDetails (_:children) = map (\y -> trimTrailingComa y) children
          trimTrailingComa  text = if (last text==',') then init text else text

          
--locateNode name input = (\(x,y,z) -> (x,y,map (\w -> w) z)) $ Data.List.find (\(x,y,z) -> x==name) input
locateNode name input = (\(name,weight,children) -> 
                    (Node name weight (map (\w -> locateNode w input) children))) 
                        $ fromJust $ Data.List.find (\(x,y,z) -> x==name) input
--locateNode name input = Data.List.find (\(x,y,z) -> x==name) input

input = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"
 
--findBottom input = findChildren $ filter (\(key,weight,children) -> (length children) > 0) $ parseTreeString input
--    where children = findChildren 
findBottom (Node name weight []) = name
findBottom (Node name weight children) = findBottom (last children)

findChildren input =concat $  (\y -> map (\(key, age, children) -> children) y) input

findHead input = head $ filter (\y -> (elem (fromString y) (findChildren input) == False))  $ 
    map (\(key,_,_) -> key) input

day7b :: String -> Int
day7b input = 0

data Tree a =  Node String Int [Tree a] deriving (Show, Read, Eq)