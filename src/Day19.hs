module Day19
    ( 
    day19
   ,day19b
    )
    where
import Data.List
    
day19 :: String -> String 
day19 input =   filter(\y-> elem y ['A'..'Z'] ) $ takeWhile (/=' ') $ map (\(Step (_,a,grid)) ->characterAt a grid) $  iterate traverseRoute $ findStart $ grid
    where grid= lines input
    
day19b input =   length$ takeWhile (/=' ') $ map (\(Step (_,a,grid)) ->characterAt a grid) $  iterate traverseRoute $ findStart $ grid
    where grid= lines input
    
parseInput input = lines input

data Direction = North | South | East | West deriving (Eq, Show)

--findStart :: [String] -> (Direction, (Int,Int), [String])
findStart grid = case (findIndex (=='|') (head $ grid)) of 
                    Just start -> Step (South, (start,0), grid)
                    Nothing -> error "No start found"
    
data Step = Step (Direction, (Int,Int), [[Char]]) deriving (Eq, Show)

--traverseRoute (dir, (x,y), grid)
traverseRoute (Step (dir, (x,y), grid))
    | loc == '+' = findNext dir (x,y) grid
    | dir==South = Step (dir, (x,y+1), grid)
    | dir==North = Step (dir, (x,y-1), grid)
    | dir==West  = Step (dir, (x-1,y), grid)
    | dir==East  = Step (dir, (x+1,y), grid)
    where loc = characterAt(x,y) grid

characterAt:: (Int, Int) -> [String] -> Char
characterAt(x,y) grid 
    | x >= gridWidth  ||  y >= gridHeight  = ' '
    | x < 0||  y < 0 = ' '
    | otherwise = ((grid !! y) !! x) 
    where (gridWidth, gridHeight) = ((length $ head grid), (length grid))


adjacent :: Direction -> (Int,Int) -> [(Direction, (Int,Int))]
adjacent  direction (x,y)
    | direction == North = filter (\y -> fst y /= South ) adj
    | direction == South = filter (\y -> fst y /= North ) adj
    | direction == East  = filter (\y -> fst y /= West )  adj
    | direction == West  = filter (\y -> fst y /= East )  adj
    where adj = [(East,(x+1,y)),(West,(x-1,y)),(South,(x,y+1)),(North,(x,y-1))]


--findNext dir (x,y) grid = (dir, (x,y), grid)
findNext dir (x,y) grid = case (find (\z-> (characterAt(snd z) grid) /= ' ') $ adjacent dir (x,y)) of 
                            Just (nextDir, next)  -> Step (nextDir, next, grid)
                            Nothing -> error "lost the trail"


grid = lines input
input = "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ "
{--
01234567890123456
     |          0
     |  +--+    1
     A  |  C    2
 F---|----E|--+ 3
     |  |  |  D 4
     +B-+  +--+ 5

--}