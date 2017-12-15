module Day9Spec where
import Day9
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day9" $ do
        it "should something"  $ do 
            let input = ""
            let expected = 1
            day9 input `shouldBe` expected
