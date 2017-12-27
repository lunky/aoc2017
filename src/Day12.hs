module Day12
    ( 
    day12
   ,day12b
    )
    where
import Data.Maybe (fromJust)
import Data.List (foldl',find,partition)
import qualified Data.Set as Set

day12 :: String -> Int
day12 input = fromJust $ fmap length $ find (Set.member "0") $ findConnected $ parseTree input

findConnected :: (Foldable t,Ord a) => t(a,[a]) -> [Set.Set a]
findConnected theSet = foldl' connect [] theSet

parseTree :: String -> [(String,[String])]
parseTree input = map parseLine $ lines input

parseLine :: String -> (String,[String])
parseLine input = (\list -> (head list, (tail $ tail list)) ) $ map trimTrailingComma $words input 

trimTrailingComma :: String -> String
trimTrailingComma  text = if (last text==',') then init text else text

connect :: Ord a => [Set.Set a] -> (a, [a]) -> [Set.Set a]
connect sets (key, children) = Set.unions (theSet : connected) : isolated
  where theSet = Set.fromList (key : children)
        (isolated, connected) = partition (Set.null . Set.intersection theSet) sets


day12b :: String -> Int
day12b input = length . foldl' connect [] $ parseTree input

