module Day9 
    ( 
    day9
   ,day9b
    )
    where
day9 :: String -> Int
day9 input = parseGroup 0 0 input
    
day9b :: String -> Int
day9b input = countGarbage 0 False input

parseGroup :: Int -> Int -> String -> Int
parseGroup count score ('{':input) = parseGroup (count + 1) (score + count + 1) input
parseGroup count score ('}':input) = parseGroup (count - 1) score  input
parseGroup count score ('<':input) = parseGarbage count score  input
    where parseGarbage :: Int -> Int -> String -> Int
          parseGarbage count score ('!':_:input) = parseGarbage count score input
          parseGarbage count score ('>':input) = parseGroup count score input
          parseGarbage count score (_:input) = parseGarbage count score input
          parseGarbage _ score [] = score
          
parseGroup count score  (_:input) = parseGroup count score  input
parseGroup count score [] = score
    

countGarbage count inBlock ('!':_:input) = countGarbage count inBlock input
countGarbage count True ('<':input) = countGarbage (count+1) True input
countGarbage count inBlock ('<':input) = countGarbage count True input
countGarbage count inBlock ('>':input) = countGarbage (count) False input
countGarbage count inBlock (_:input) = countGarbage (if inBlock then (count +1) else count) inBlock input
countGarbage count _ [] = count 