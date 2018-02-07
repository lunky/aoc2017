module Day16
    ( 
    day16
   ,day16b
   ,spin
   ,exchange
   ,partner
   ,parseLine
   ,Instruction(..)
    )
    where
import Data.Maybe (fromJust)
import Data.List (elemIndex,foldl')
import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Set (member)
import qualified Data.Set as S
    

data Instruction = Spin Int | Exchange Int Int | Partner Char Char deriving (Show, Eq)

day16 :: String -> String -> String
day16 instructions programs = fromJust $ fmap (foldl' (\acc y-> doTheThing acc y) programs)  $ parseLine instructions

day16b :: Integer -> String -> String -> String
day16b count instructions programs = do
    let fullSet = takeUntilDuplicate $ iterate (day16 programs) instructions
    let idx = count `rem` (toInteger $ length fullSet)
    fullSet !! (fromIntegral idx)

spin :: Int -> [a] -> [a]
spin inst input = take overall $ drop  (overall - inst) $ cycle input
    where overall = length input
    
exchange :: (Int,Int) -> [a] -> [a]
exchange (from,to) input 
    | from == to = input
    | otherwise = swap' (min from to) (max from to) input
    where swap' :: Int -> Int -> [a] -> [a]
          swap' first second lst = beginning ++ [y] ++ middle ++ [x] ++ end
            where (beginning, (x : r)) = splitAt first lst
                  (middle, (y : end)) = splitAt (second - first - 1) r
                  
partner :: (Eq a)=> (a,a)-> [a] -> [a]
partner (from,to) input = exchange ( fromJust $ elemIndex from input, fromJust $ elemIndex to input) input

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

parseSpin:: ReadP Instruction
parseSpin = do
    string "s"
    howMany<- many1 digit
    return $ Spin (read howMany)

parseExchange :: ReadP Instruction
parseExchange = do
    string "x"
    x <- many1 digit
    string "/"
    y <- many1 digit
    return $ Exchange (read x) (read y)
    
parsePartner:: ReadP Instruction
parsePartner = do
    string "p"
    x <- get
    string "/"
    y <- get
    return $ Partner  x y


commaSep p  = p `sepBy` (char ',')

parseLine :: String -> Maybe [Instruction]
parseLine input = parseMaybe (sepBy1  (parsePartner<|>parseExchange<|>parseSpin) (char ','))  input

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        result  -> Just $ fst $ last result

doTheThing :: String -> Instruction -> String
doTheThing input (Spin x) = spin x input
doTheThing input (Partner x y) = partner (x,y) input
doTheThing input (Exchange x y) = exchange (x,y) input

takeUntilDuplicate :: Ord a => [a] -> [a]
takeUntilDuplicate xs = foldr go (const []) xs S.empty
  where
    go x cont set
      | x `member` set = []
      | otherwise      = x : cont (S.insert x set)