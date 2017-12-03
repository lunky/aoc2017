module Day3Spec where
import Day3
import Lib
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day3" $ do
        it "should give a response"  $ do 
            let expected = 0
            let input = 1
            day3 input `shouldBe` expected
        it "should give a response 12 3"  $ do 
            let expected = 3
            let input = 12
            day3 input `shouldBe` expected
    describe "Day3b" $ do
        it "should give a response"  $ do 
            let expected = 0
            let input = 0
            day3b input `shouldBe` expected
            
        