module Day20Spec where

import Day20
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $ do
            1 `shouldBe` 1
    describe "parseInput" $ do
        it "should parse the input" $ do
            let input = "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>\n"
            let expected = ((3,0,0), (2,0,0), (-1,0,0))
            parseInput input `shouldBe` expected
            
    describe "Day20" $ do
        it "should choose particle 0" $ do 
            let input = testInput
            let expected = 0
            day20 input `shouldBe` expected
--            1 `shouldBe` 1
    
testInput = "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>\np=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"