module Day8 
    ( 
    day8
   ,day8b
   ,calculate
    )
    where
import qualified Data.Map as Map

day8 :: String -> Int
day8 input = do 
    let theMap = Map.empty
    pickMax $ foldr (\y registers-> calculate (words y) registers) theMap $ lines input
--day8 input = 0
pickMax :: Map.Map String Int -> Int
pickMax input = maximum $ map (\(key,val) -> val) $ Map.toList input

-- "b inc 5 if a > 1"
--calculate :: [a] -> Map.Map String a -> Map.Map String a
calculate (register:[operation,val,_,testRegister,comparison,comparisonValue]) registers = do
    let testValue = getRegister testRegister registers
    let registerValue = getRegister register registers
    if( testIf comparison testValue (read comparisonValue)) then
        setRegister register (incOrDec operation registerValue (read val)) registers
    else
        registers

setRegister :: String -> Int -> Map.Map String Int -> Map.Map String Int
setRegister testRegister newVal registers = Map.insertWith (+) testRegister newVal registers

getRegister :: String -> Map.Map String Int -> Int
getRegister testRegister registers = case (Map.lookup testRegister registers) of 
                                        Just r -> r
                                        Nothing -> 0

input = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"

incOrDec :: String -> Int -> Int -> Int
incOrDec "dec" x y = x - y
incOrDec "inc" x y = x + y

testIf :: String -> Int -> Int -> Bool
testIf "<"   x y = x <  y
testIf ">"   x y = x >  y
testIf "=="  x y = x == y
testIf "<="  x y = x <= y
testIf ">="  x y = x >= y
testIf "!="  x y = x /= y

day8b :: String -> Int
day8b input = 0
