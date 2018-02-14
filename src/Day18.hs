module Day18
    ( 
    day18
   ,day18b
   ,parseInput
   ,doTheThing
   ,doAllTheThings
   ,Instruction(..)
    )
    where
import Text.ParserCombinators.ReadP
import Control.Applicative  ((<|>))
import Data.Maybe
import Data.Map  hiding ((!))
import qualified Data.Map as Map

    
day18 :: String -> Int 
day18 input =  (doAllTheThings $ parseInput input) ! '+'

day18b :: String -> Int
day18b input = 0

doAllTheThings :: [Instruction] -> Map Char Int
doAllTheThings [] = Data.Map.empty
doAllTheThings instructions = playInstructions instructions 0 Data.Map.empty
    where  playInstructions instructions current registers 
                | registers!'+' /= 0 = registers
                | instructions == [] = error "empty instructions"
                | current < 0 || current > (length instructions - 1) = registers
                | otherwise = playInstructions instructions (nextIndex) newRegisters
                    where (newRegisters,nextIndex) = 
                                doTheThing (instructions,current) ( instructions !! current ) registers

infixl 9 ! --
(!) :: Map Char Int -> Char -> Int 
m ! k    = findWithDefault 0 k m

doTheThing :: ([Instruction],Int) -> Instruction -> Map Char Int -> (Map Char Int,Int)
doTheThing (instructions,current) (Set register val) registers = (insert register val registers,current+1)
doTheThing (instructions,current) (SetRef register val) registers = (insert register (registers ! val) registers , current+1)
doTheThing (instructions,current) (Add register val) registers = (insertWith (+) register val registers , current+1)
doTheThing (instructions,current) (AddRef register val) registers = (insertWith (+) register (registers ! val) registers , current+1)
doTheThing (instructions,current) (Mul register val) registers = (update mult register registers , current+1)
    where  mult el = Just (val * el)
doTheThing (instructions,current) (MulRef register val) registers = (update mult register registers , current+1)
    where  mult el = Just ((registers!val) * el)
doTheThing (instructions,current) (Mod register val) registers = (update md register registers , current+1)
    where  md el = Just (el `mod` val)
doTheThing (instructions,current) (ModRef register val) registers = (update md register registers , current+1)
    where  md el = Just (el `mod` (registers!val) )
    
doTheThing (instructions,current) (Snd register) registers = (insert '=' (registers ! register) registers, current+1)

doTheThing (instructions,current) (Rcv register) registers 
    | ((registers!register) == 0) = (registers,current+1)
    | otherwise = (insert '+' (registers!'=') registers,current+1) -- non zero flag for test 
    
doTheThing (instructions,current) (Jgz register val) registers 
    | ((registers!register) == 0) = (registers,current+1)
    | otherwise = (registers,current+val)

doTheThing (instructions,current) (JgzRef register el) registers 
    | ((registers!register) == 0) = (registers,current+1)
    | otherwise = (registers,current+ (registers!el))

-- alterF :: (Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)


parseInput :: String -> [Instruction]
parseInput input = case (parseProper) of 
                    Nothing -> []
                    Just (result) -> result
    where parseProper = 
            parseMaybe ( sepBy1 
            (   parseMulRef<|>parseMul<|>
                parseSetRef<|>parseSet<|>
                parseAddRef<|>parseAdd<|>
                parseModRef<|>parseMod<|>
                parseJgzRef<|>parseJgz<|>
                parseRcv   <|>parseSnd) 
            (char '\n')) input

parseJgz:: ReadP Instruction
parseJgz= do
    string "jgz "
    register <- get
    string " "
    val <- many1 digit 
    return (Jgz register (read val))
    
parseJgzRef:: ReadP Instruction
parseJgzRef= do
    string "jgz "
    register <- get
    string " "
    val <- get
    return (JgzRef register val)

parseMod:: ReadP Instruction
parseMod = do
    string "mod "
    register <- get
    string " "
    val <- many1 digit 
    return (Mod register (read val))
    
parseModRef:: ReadP Instruction
parseModRef = do
    string "mod "
    register <- get
    string " "
    val <- get
    return (ModRef register val)
    
parseSet:: ReadP Instruction
parseSet = do
    string "set "
    register <- get
    string " "
    val <- many1 digit 
    return (Set register (read val))
    
parseSetRef:: ReadP Instruction
parseSetRef = do
    string "set "
    register <- get
    string " "
    val <- get
    return (SetRef register val)
    
parseMul:: ReadP Instruction
parseMul = do
    string "mul "
    register <- get
    string " "
    val <- many1 digit 
    return (Mul register (read val))

parseMulRef:: ReadP Instruction
parseMulRef = do
    string "mul "
    register <- get
    string " "
    val <- get
    return (MulRef register val)

parseAdd:: ReadP Instruction
parseAdd = do
    string "add "
    register <- get
    string " "
    val <- many1 digit 
    return (Add register (read val))

parseAddRef:: ReadP Instruction
parseAddRef = do
    string "add "
    register <- get
    string " "
    val <- get
    return (AddRef register val)

parseRcv:: ReadP Instruction
parseRcv = do
    string "rcv "
    register <- get
    return (Rcv register)

parseSnd:: ReadP Instruction
parseSnd = do
    string "snd "
    register <- get
    return (Snd register)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        result  -> Just $ fst $ last result

digit :: ReadP Char
digit = satisfy (\char -> (char >= '0' && char <= '9') || char=='-' )

data Instruction =  Set Char Int | SetRef Char Char
                  | Add Char Int | AddRef Char Char
                  | Mul Char Int | MulRef Char Char
                  | Mod Char Int | ModRef Char Char
                  | Jgz Char Int | JgzRef Char Char 
                  | Rcv Char     | Snd Char     
                  deriving (Show, Eq)
input = "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 680\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19"

