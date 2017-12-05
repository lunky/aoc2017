module Day5Spec where
import Day5
import Lib
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day5" $ do
        it "should give a step count of 5"  $ do 
            let expected = 5 
            let input = "0\n3\n0\n1\n-3"
            day5 input `shouldBe` expected
    describe "Day5b" $ do
        it "should give a step count of 10"  $ do 
            let expected =  10
            let input = "0\n3\n0\n1\n-3"
            day5b input `shouldBe` expected
