module Day8Spec where
import Day8
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day8" $ do
        it "should calculate the value of the greatest register"  $ do 
            let input = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"
            let expected = 1
            day8 input `shouldBe` expected
