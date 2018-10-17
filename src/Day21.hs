module Day21
    ( 
    day21
   ,day21b
   ,parseRules
   ,matches
   ,enhance
   ,seed
   ,Rule(..)
    )
    where
    
import Text.ParserCombinators.ReadP
import Data.List (transpose)
import Data.Maybe (catMaybes)

seed = ".#.\n..#\n###"
input = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

data Rule = Rule ([String], [String]) deriving (Show)

parseRules :: String -> [Rule]
parseRules input = catMaybes $ map (\y -> parseRule y) $ lines input

parseRule :: String -> Maybe Rule
parseRule line 
    | length line == 20 = parseMaybe parseShortRule line
    | otherwise = parseMaybe parseLongRule line

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        result  -> Just $ fst $ last result
day21 :: Int -> String -> Int
day21 iterations input = 0 -- parseInput input 
{--
day21 input = parseInput iterations input
--}

--parseInput :: String -> [(String,String)]
parseInput input = lines input
-- ##/## => #.#/#../###
-- .../.../... => ..../#.../.##./..#.


parseShortRule :: ReadP Rule
parseShortRule = do
    a <- many1 pixel
    string "/"
    b <- many1 pixel
    string " => "
    c <- many1 pixel
    string "/"
    d <- many1 pixel
    string "/"
    e <- many1 pixel
    return $ Rule ([a,b],[c,d,e])

parseLongRule :: ReadP Rule
parseLongRule = do
    a <- many1 pixel
    string "/"
    b <- many1 pixel
    string "/"
    c <- many1 pixel
    string " => "
    d <- many1 pixel
    string "/"
    e <- many1 pixel
    string "/"
    f <- many1 pixel
    string "/"
    g <- many1 pixel
    
    return $ Rule ([a,b,c],[d,e,f,g])

ruleSize :: Rule -> Int
ruleSize (Rule (from,to)) = length from

matches :: Rule -> [String] -> Bool
matches (Rule (from ,to)) inputs = anyMatch from inputs
    where size=length inputs
          allMatch rule inputs = all (\(a,b) -> a==b) $ zip rule inputs
          anyMatch rule inputs = any (\input -> allMatch rule input) [inputs, map reverse inputs, transpose inputs, map reverse $ transpose inputs]
          
pixel :: ReadP Char
pixel = satisfy (\char -> (char=='.' || char=='#' ))

day21b :: String -> Int
day21b input = 0

enhance :: String -> [String] -> [Bool]
enhance rules pattern = map (\r -> matches r pattern) rules'
    where rules' = parseRules rules

{--
##.
#..
...

.##
..#
...

...
..#
.##
--}