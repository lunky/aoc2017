module Day6Spec where
import Day6
import Lib
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day6" $ do
        it "should give a step count of 5"  $ do 
            let expected = 5 
            let input = "0\t2\t7\n0"
            day6 input `shouldBe` expected
