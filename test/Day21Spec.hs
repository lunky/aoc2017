module Day21Spec where

import Day21
import Test.Hspec

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
--    describe "enhance" $ do
--        it " should match produce the output from sample 1" $ do
--            let input = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
--            let pattern = [".#.","..#","###"]
--            let expected = ["#..#","....","....","#..#"]
--            enhance input pattern `shouldBe` expected
    describe "enhance" $ do
        it "should transform the first example" $ do
            let input = ""
            let input2 = [] 
            let output = ""
            let expected = [False]
            enhance input input2 `shouldBe` expected
        
    
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
