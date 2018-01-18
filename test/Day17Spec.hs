module Day17Spec where

import Day17
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $ do
            1 `shouldBe` 1
    describe "advance" $ do
        it "should follow the minimum example (0)" $ do
            let input = 1
            let state =  (0,[0])
            let expected =  (1,[0,1])
            advance 3 state input `shouldBe` expected
        it "should follow the minimum example (1,[0,1])" $ do
            let input = 2
            let state =  (1,[0,1])
            let expected =  (1,[0,2,1])
            advance 3 state input `shouldBe` expected
        it "should follow the example (1,[0,2,1])" $ do
            let input = 3
            let state =  (1,[0,2,1])
            let expected =  (2,[0,2,3,1])
            advance 3 state input `shouldBe` expected
        it "should follow the example (2,[0,2,3,1])" $ do
            let input = 4
            let state =  (2,[0,2,3,1])
            let expected =  (2,[0,2,4,3,1])
            advance 3 state input `shouldBe` expected
        it "should follow the example (2,[0,2,3,1])" $ do
            let input = 5
            let state =  (2,[0,2,4,3,1])
            let expected =  (1,[0,5,2,4,3,1])
            advance 3 state input `shouldBe` expected
            
            
    describe "Day17" $ do
        it "should produce the next number" $ do
            pendingWith "too slow to run"
            let input = 3
            let expected = 638
            day17 input `shouldBe` expected
