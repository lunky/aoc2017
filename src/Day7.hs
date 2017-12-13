{-# LANGUAGE OverloadedStrings #-}
module Day7 
    ( 
    day7
   ,day7b
   ,parseTreeString
   ,findChildren
   ,locateNode
   ,buildTree 
   ,findUnbalanced
   ,childWeights
    )
    where
import Data.Function (on)
import Data.String (fromString)
import Data.List (find, groupBy, group, nub, sortOn, sort)
import Data.Maybe

data Tree a = Node String Int [Tree a] deriving (Show, Read, Eq)

day7 :: String -> String
day7 input = findHead $ parseTreeString input

parseTreeString input = map (\(program:details) -> (program, (\y->(read y)::Int) $ 
    init $ drop 1 $ head details, parseDetails $ tail details) ) $
    map words $ lines input
    where parseDetails []=[]
          parseDetails (_:children) = map (\y -> trimTrailingComa y) children
          trimTrailingComa  text = if (last text==',') then init text else text

          
locateNode name input = (\(name,weight,children) -> 
                    (Node name weight (map (\w -> locateNode w input) children))) 
                        $ fromJust $ Data.List.find (\(x,y,z) -> x==name) input
 
findChildren input =concat $  (\y -> map (\(key, age, children) -> children) y) input

findHead input = head $ filter (\y -> (elem (fromString y) (findChildren input) == False))  $ 
    map (\(key,_,_) -> key) input

day7b :: String -> Int
day7b input =  (\tree-> balanceDifference tree + ((\(Node name weight children) -> weight)$ last $ findUnbalanced tree) ) $ buildTree input
    where balanceDifference input = (\(x:[y])->x-y) $ nub $ concat $ weights $ kids $ input
          weights children = group. sort . map (\y -> (getWeight y)) $ children
          kids (Node node weight children) =  children

findUnbalanced :: Tree a -> [Tree a]
findUnbalanced (Node name weight children) =
      let unbalancedNode = snd $ head $ head $ filter(\y->length y==1)$ childWeights children
  in if length (childWeights children)> 1
    then (unbalancedNode : findUnbalanced unbalancedNode)
    else []
    
childWeights children = groupBy (\x y -> fst x==fst y) . sortOn (\(x,y)->x) . map (\y -> (getWeight y,y)) $ children

buildTree input = locateNode (findHead $ parseTreeString input) $ parseTreeString input

getWeight (Node name weight children) = weight + (sum $ map (\y -> getWeight y) children)



