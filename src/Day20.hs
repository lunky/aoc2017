module Day20
    ( 
    day20
   ,day20b
   ,parseInput
    )
    where
    
import Data.Function (on);
import Data.Maybe (fromJust);
import Data.List (foldl1',elemIndex,groupBy);
import Text.ParserCombinators.ReadP
    
-- problem wants which particle has the lowest acceleration - perform a tick 
-- and get the index of the particle with the smallest acceleration
day20 :: String -> Int 
day20 input = fromJust $ (\y -> (elemIndex $ (foldl1' min y)) y) $  map (\(_,_,accel) -> manhattan accel) $ map tick $ fmap parseInput (lines  input)

-- brute force run 500 ticks removing anything with a duplicate position..( 500 worked for me YMMV )
day20b :: String -> Int
day20b input = last $ take 500 $ map length $ iterate ( removeCollisions . map tick) $ fmap parseInput (lines  input)

manhattan :: (Int,Int,Int) -> Int
manhattan (a,b,c) = abs a + abs b + abs c

parseInput :: String -> ((Int,Int,Int), (Int,Int,Int), (Int,Int,Int))
parseInput input = case (parseMaybe parseTick input) of
                    Nothing -> error "invalid input"
                    Just(result) -> result

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        result  -> Just $ fst $ last result

parseTick :: ReadP ((Int,Int,Int), (Int,Int,Int), (Int,Int,Int))
parseTick = do
    string "p="
    skipSpaces 
    pos <- parseTriplet
    string ", v="
    vel <- parseTriplet
    string ", a="
    acc <- parseTriplet
    return (pos,vel,acc)
    
--    < 3,0,0>
parseTriplet :: ReadP (Int,Int,Int)
parseTriplet = do
    string "<"
    skipSpaces 
    a <- many1 digit
    string ","
    skipSpaces 
    b <- many1 digit
    string ","
    skipSpaces 
    c <- many1 digit
    string ">"
    return (read a,read b,read c)

digit :: ReadP Char
digit = satisfy (\char -> (char >= '0' && char <= '9') || char=='-' )

{--
Increase the X velocity by the X acceleration.
Increase the Y velocity by the Y acceleration.
Increase the Z velocity by the Z acceleration.
Increase the X position by the X velocity.
Increase the Y position by the Y velocity.
Increase the Z position by the Z velocity.
--}
tick :: ((Int,Int,Int), (Int,Int,Int), (Int,Int,Int)) -> ((Int,Int,Int), (Int,Int,Int), (Int,Int,Int))
tick (position, velocity, acceleration) =  ( newPosition, newVelocity, acceleration)
    where increase (a,b,c) (d,e,f) = (a+d, b+e, c+f)
          newVelocity = increase velocity acceleration
          newPosition = increase position newVelocity

removeCollisions :: [((Int,Int,Int),(Int,Int,Int),(Int,Int,Int))] -> [((Int,Int,Int),(Int,Int,Int),(Int,Int,Int))]
removeCollisions = concat . filter ((== 1) . length) . 
                            groupBy (\(pos,_,_) (pos2,_,_) -> pos == pos2)


input :: [((Int,Int,Int),(Int,Int,Int),(Int,Int,Int))]
input = [(( 3,0,0), ( 2,0,0), (-1,0,0)),(( 4,0,0), ( 0,0,0), (-2,0,0))]

input2 :: [((Int,Int,Int),(Int,Int,Int),(Int,Int,Int))]
input2 = [((-6,0,0), ( 3,0,0), ( 0,0,0)),    ((-4,0,0), ( 2,0,0), ( 0,0,0)),((-2,0,0), ( 1,0,0), ( 0,0,0)),(( 3,0,0), (-1,0,0), ( 0,0,0))]