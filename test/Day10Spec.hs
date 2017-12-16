module Day10Spec where
import Day10
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day10" $ do
        it "should parse single"  $ do 
            let input = "{}"
            let expected = 1
            day10 input `shouldBe` expected
