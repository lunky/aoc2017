module Day1Spec where

import Day1
import Lib
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $
            1 `shouldBe` 1
    describe "Day1" $ do
        it "should get 3 from 1122" $ do
            let expected = 3
            let input = "1122"
            day1 input `shouldBe` expected
        it "should get 4 from 1111" $ do
            let expected = 4
            let input = "1111"
            day1 input `shouldBe` expected
        it "should get 0 from 1234"  $ do 
            let expected = 0
            let input = "1234"
            day1 input `shouldBe` expected
        it "should get 0 from 1234"  $ do 
            let expected = 9
            let input = "91212129"
            day1 input `shouldBe` expected
    describe "Day1b" $ do
        it "should get 6 from 1212"  $ do 
            let expected = 6
            let input = "1212"
            day1b input `shouldBe` expected
        it "should get 4 from 12345"  $ do 
            let expected = 4
            let input = "123425"
            day1b input `shouldBe` expected
        it "should get 12 from 12345"  $ do 
            let expected = 12
            let input = "123123"
            day1b input `shouldBe` expected
        it "should get 4 from 12131415"  $ do 
            let expected = 4
            let input = "12131415"
            day1b input `shouldBe` expected
