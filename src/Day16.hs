module Day16
    ( 
    day16
   ,day16b
   ,spin
   ,exchange
   ,partner
   ,spinv
   ,exchangev
   ,partnerv
   ,parseLine
   ,Instruction(..)
    )
    where
import Data.Maybe (fromJust)
import Data.List (elemIndex,foldl')
import Text.ParserCombinators.ReadP
import Control.Applicative
import Data.Vector ((!), (//))
import qualified Data.Vector as V
    

data Instruction = Spin Int | Exchange Int Int | Partner Char Char deriving (Show, Eq)

--day16 :: String -> String -> String
--day16 programs instructions = fromJust $ fmap (foldl' (\acc y-> doTheThing acc y) programs)  $ parseLine instructions
day16 :: String -> String -> String
day16 programs instructions = V.toList $ fromJust $ fmap (foldl' (\acc y-> doTheThingv acc y) (V.fromList programs))  $ parseLine instructions

day16' :: V.Vector Char -> [Instruction] -> V.Vector Char
day16' programs instructions =  (foldl' (\acc y-> doTheThingv acc y) programs instructions)

day16b :: Integer -> String -> String -> String
day16b count programs instructions = V.toList $ foldl' (\acc y -> day16' acc  insts) (V.fromList programs) [1..count]
                where insts = fromJust $ parseLine instructions

spin :: Int -> [a] -> [a]
spin inst input = take overall $ drop  (overall - inst) $ cycle input
    where overall = length input
    
spinv :: Int -> V.Vector Char -> V.Vector Char
spinv inst input =  V.concat [(V.slice start (inst) input), (V.slice 0 start input)]
    where start = (length input) - inst

exchange :: (Int,Int) -> [a] -> [a]
exchange (from,to) input 
    | from == to = input
    | otherwise = swap' (min from to) (max from to) input
    where swap' :: Int -> Int -> [a] -> [a]
          swap' first second lst = beginning ++ [y] ++ middle ++ [x] ++ end
            where (beginning, (x : r)) = splitAt first lst
                  (middle, (y : end)) = splitAt (second - first - 1) r
                  
exchangev:: (Int,Int) -> V.Vector  Char -> V.Vector Char
exchangev (from,to) input = 
      let x = input ! from
          y = input ! to 
      in input  // [(from, y), (to, x)]

partner :: (Eq a)=> (a,a)-> [a] -> [a]
partner (from,to) input = exchange ( fromJust $ elemIndex from input, fromJust $ elemIndex to input) input

partnerv :: (Char, Char) -> V.Vector Char -> V.Vector Char
partnerv (from,to) input =
    let i = fromJust $ V.findIndex (==from) input 
        j = fromJust $ V.findIndex (==to) input 
    in exchangev (i, j) input

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

parseSpin:: ReadP Instruction
parseSpin = do
    string "s"
    howMany<- many1 digit
    return $ Spin (read howMany)
    
--    s2,x5/15,pf/a,x12/10,pp/h,

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

doTheThingv :: V.Vector Char -> Instruction -> V.Vector Char
doTheThingv input (Spin x) = spinv x input
doTheThingv input (Partner x y) = partnerv (x,y) input
doTheThingv input (Exchange x y) = exchangev (x,y) input

--  data Instruction = Spin Int | Exchange Int Int | Partner Char Char deriving (Show, Eq)

--surface :: Shape -> Float  
--surface (Circle _ _ r) = pi * r ^ 2  
--aisurface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  