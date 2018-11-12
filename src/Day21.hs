module Day21
    ( 
    day21
   ,day21b
   ,parseRules
   ,matches
   ,firstMatch
   ,enhance
   ,iteration
   ,seed
   ,divideSquares
   ,Rule(..)
    )
    where
    
import Text.ParserCombinators.ReadP
import Data.List (transpose, find)
import Data.Maybe (Maybe, catMaybes)

seed = ".#.\n..#\n###"
input = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

data Rule = Rule ([String], [String]) deriving (Show)

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n
  build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
  build g = g (:) []

divideSquares size square = concat $ transpose $ map ( chunksOf size ) $ transpose $ map ( chunksOf size) square

undivideSquares size square = concatMap transpose $ chunksOf size square

firstMatch :: [Rule] -> [String] -> Maybe [String]
firstMatch rules line = find (\y -> matches y line) rules >>= result
    where result (Rule (a,b)) = Just b

iteration :: [Rule] -> [String] -> Maybe [String]
iteration rules line 
    | (length $ head line) `mod` 2 == 0 = Just $ concat $ undivideSquares 2 $ concat $ sequence $ fmap (firstMatch rules) $ divideSquares 2 line  
    | otherwise                         = Just $ concat $ undivideSquares 3 $ concat $ sequence $ fmap (firstMatch rules) $ divideSquares 3 line  

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
          anyMatch rule inputs = any (\input -> allMatch rule input) [inputs, map reverse inputs, reverse inputs, map reverse $ reverse inputs ]
          
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

{--
ab|cd
ef|gh
-----
ij|kl
mn|op

1   2  3|  4  5  6 
7   8  9| 10 11 12 
13 14 15| 16 17 18
------------------
19 20 21| 22 23 24
25 26 27| 28 29 30
31 32 33| 34 35 36
--}