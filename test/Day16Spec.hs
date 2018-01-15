module Day16Spec where

import Day16
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MV

spec :: Spec
spec = do
    describe "spin" $ do
        it "should spin" $ do
            let input = 1 
            let input2 = "abcde"
            let expected = "eabcd"
            spin input input2 `shouldBe` expected
            
    describe "exchange" $ do
        it "should exchange" $ do 
            let input = (3,4)
            let input2 = "eabcd"
            let expected = "eabdc"
            exchange input input2 `shouldBe` expected
            
    describe "partner" $ do
        it "should partner" $ do 
            let input = ('e','b')
            let input2 = "eabdc"
            let expected = "baedc"
            partner input input2 `shouldBe` expected
    describe "parseLine" $ do
        it "should parse string into Instructions" $ do 
            let input = "s1,x3/4,pe/b"
            let expected = Just [Spin 1, Exchange 3 4, Partner 'e' 'b']
            parseLine input `shouldBe` expected
    describe "Day16" $ do
        it "should execute all your intructions" $ do
            let input = "abcde"
            let input2 = "s1,x3/4,pe/b"
            let expected = "baedc"
            day16 input input2 `shouldBe` expected
    describe "Day16b" $ do
        it "should execute instructions 1B times" $ do
            let input = "abcde"
            let input2 = "s1,x3/4,pe/b"
            let expected = "ceadb"
            day16b 2 input input2 `shouldBe` expected

    describe "spinv" $ do
        it "should spinv" $ do
            let input = 1 
            let input2 = V.fromList "abcde"
            let expected = V.fromList "eabcd"
            spinv input input2 `shouldBe` expected
            
    describe "exchangev" $ do
        it "should exchange" $ do 
            let input = (3,4)
            let input2 = V.fromList "eabcd"
            let expected = V.fromList "eabdc"
            exchangev input input2 `shouldBe` expected
            
    describe "partnerv" $ do
        it "should partnerv" $ do 
            let input = ('e','b')
            let input2 = V.fromList "eabdc"
            let expected = V.fromList "baedc"
            partnerv input input2 `shouldBe` expected