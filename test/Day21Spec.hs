module Day21Spec where

import Day21
import Test.Hspec
import Data.Maybe

spec :: Spec
spec = do

    describe "matches" $ do
        it " should match the short sample string" $ do
            let input = lines "..\n.#"
            let rule = Rule (["..",".#"],["##.","#..","..."])
            let expected = True
            matches rule input `shouldBe` expected
        it " should match the short sample string rotated" $ do
            let input = lines "..\n#."
            let rule = Rule (["#.",".."],["##.","#..","..."])
            let expected = True
            matches rule input `shouldBe` expected
        it " should match the short sample string rotated and flipped" $ do
            let input = lines "#.\n.."
            let rule = Rule (["#.",".."],["##.","#..","..."])
            let expected = True
            matches rule input `shouldBe` expected
        it " should match the short sample string flipped" $ do
            let input = lines "..\n#."
            let rule = Rule (["#.",".."],["##.","#..","..."])
            let expected = True
            matches rule input `shouldBe` expected
        it " should match the long sample string" $ do
            let input = lines "##.\n#..\n..."
            let rule = Rule (["##.","#..","..."],[])
            let expected = True
            matches rule input `shouldBe` expected
        it " should fail to match the a string that doesn't match" $ do
            let input = lines "...\n###\n.##"
            let rule = Rule (["..",".#"],["##.","#..","..."])
            let expected = False
            matches rule input `shouldBe` expected
        it " should match a string that" $ do
            let input = lines ".#.\n..#\n###"
            let rule = Rule ([".#.","..#","###"],["#..#","....","....","#..#"])
            let expected = True
            matches rule input `shouldBe` expected
        it " should match a string that failed in my test" $ do
            let input = ["..",".#"]
            let rule = Rule (["..",".#"],["##.","#..","..."])
            let expected = True
            matches rule input `shouldBe` expected
        it " should match another string that failed in my test" $ do
            let input = ["#.",".."]
            let rule = Rule (["..",".#"],["##.","#..","..."])
            let expected = True
            matches rule input `shouldBe` expected
    describe "firstMatch" $ do
        it " should return the first match from a list" $ do
            let input = lines ".#.\n..#\n###"
            let rules = [ Rule (["#.",".."],["##.","#..","..."]),
                          Rule ([".#.","..#","#.#"],["#..#","....","....","####"]),
                          Rule ([".#.","..#","###"],["#..#","....","....","#..#"])]
            let expected = Just ["#..#","....","....","#..#"]
            firstMatch rules input `shouldBe` expected
        it " should return nothing for no matches from a list" $ do
            let input = lines ".#.\n..#\n###"
            let rules = [ Rule (["#.",".."],["##.","#..","..."]),
                          Rule (["##.","..#","#.#"],["#..#","....","....","####"]),
                          Rule (["##.","..#","###"],["#..#","....","....","#..#"])]
            let expected = Nothing
            firstMatch rules input `shouldBe` expected
    describe "iteration" $ do
        it " should return do the first 2=>3 transform" $ do
            let input = lines "#.\n.."
            let rules = [ Rule (["#.",".."],["##.","#..","..."]), Rule ([".#.","..#","###"],["#..#","....","....","#..#"])]
            let expected = Just ["##.","#..","..."]
            iteration rules input `shouldBe` expected
        it " should return do the first 3=>4 transform" $ do
            let input = lines ".#.\n..#\n###"
            let rules = [ Rule (["#.",".."],["##.","#..","..."]),
                          Rule ([".#.","..#","###"],["#..#","....","....","#..#"])]
            let expected = Just ["#..#","....","....","#..#"]
            iteration rules input `shouldBe` expected
        it " should return do the first 4=>6 transform" $ do
            -- ../.# => ##./#../...
            -- .#./..#/### => #..#/..../..../#..#
            let input = ["#..#","....","....","#..#"]
            let rules = [ Rule (["..",".#"],["##.","#..","..."]), Rule ([".#.","..#","###"],["#..#","....","....","#..#"])]
            let expected = Just ["##.","##.","#..","#..","...","...","##.","##.","#..","#..","...","..."]
            iteration rules input `shouldBe` expected
    describe "divideSquares" $ do
        it "should split 4x4 into 4x 2x2" $ do
          let input = 2
          let input2 = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
          let expected = [[[1,2], [5,6]],[[3,4],[7,8]],[[9,10],[13,14]],[[11,12],[15,16]]]
          divideSquares input input2 `shouldBe` expected
        it "should split 6x6 into 4x 3x3" $ do
          let input = 3
          let input2 = [[ 1,  2,  3,  4,  5,  6],
                        [ 7,  8,  9, 10, 11, 12],
                        [13, 14, 15, 16, 17, 18],
                        [19, 20, 21, 22, 23, 24],
                        [25, 26, 27, 28, 29, 30],
                        [31, 32, 33, 34, 35, 36]]
          let expected = [ [[1,2,3],[7,8,9],[13,14,15]],
                           [[4,5,6],[10,11,12],[16,17,18]],
                           [[19,20,21],[25,26,27],[31,32,33]],
                           [[22,23,24],[28,29,30],[34,35,36]]
                         ]
          divideSquares input input2 `shouldBe` expected
        it "should split 12x12 into 4x 3x3" $ do
          let input = 3
          let input2 =  [ [  1, 2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12],
                          [ 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24],
                          [ 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36],
                          [ 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48],
                          [ 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60],
                          [ 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72],
                          [ 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84],
                          [ 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96],
                          [ 97, 98, 99,100,101,102,103,104,105,106,107,108],
                          [109,110,111,112,113,114,115,116,117,118,119,120],
                          [121,122,123,124,125,126,127,128,129,130,131,132],
                          [133,134,135,136,137,138,139,140,141,142,143,144]]
          let expected = [
                        [[  1,  2,  3], [ 13, 14, 15], [ 25, 26, 27]],
                        [[  4,  5,  6], [ 16, 17, 18], [ 28, 29, 30]], 
                        [[  7,  8,  9], [ 19, 20, 21], [ 31, 32, 33]],
                        [[ 10, 11, 12], [ 22, 23, 24], [ 34, 35, 36]],
                        [[ 37, 38, 39], [ 49, 50, 51], [ 61, 62, 63]],
                        [[ 40, 41, 42], [ 52, 53, 54], [ 64, 65, 66]],
                        [[ 43, 44, 45], [ 55, 56, 57], [ 67, 68, 69]], 
                        [[ 46, 47, 48], [ 58, 59, 60], [ 70, 71, 72]],
                        [[ 73, 74, 75], [ 85, 86, 87], [ 97, 98, 99]],
                        [[ 76, 77, 78], [ 88, 89, 90], [100,101,102]],
                        [[ 79, 80, 81], [ 91, 92, 93], [103,104,105]],
                        [[ 82, 83, 84], [ 94, 95, 96], [106,107,108]],
                        [[109,110,111], [121,122,123], [133,134,135]],
                        [[112,113,114], [124,125,126], [136,137,138]],
                        [[115,116,117], [127,128,129], [139,140,141]], 
                        [[118,119,120], [130,131,132], [142,143,144]]]
          divideSquares input input2 `shouldBe` expected
--    describe "enhance" $ do
--        it " should match produce the output from sample 1" $ do
--            let input = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
--            let pattern = [".#.","..#","###"]
--            let expected = ["#..#","....","....","#..#"]
--            enhance input pattern `shouldBe` expected
    -- describe "enhance" $ do
    --     it "should transform the first example" $ do
    --         let input = ""
    --         let input2 = [] 
    --         let output = ""
    --         let expected = [False]
    --         enhance input input2 `shouldBe` expected
    
        
    
    describe "Day21" $ do
        it "should contain 4 pixels that are on" $ do
            pendingWith "not implemented"
            let input = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
            let expected = 4
            let iterations = 1
            day21 iterations input `shouldBe` expected
        it "should contain 12 pixels that are on" $ do
            pendingWith "not implemented"
            let input = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
            let expected = 12
            let iterations = 2
            day21 iterations input `shouldBe` expected
