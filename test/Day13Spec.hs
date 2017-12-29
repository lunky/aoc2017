module Day13Spec where
import Day13
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day13" $ do
    it "run a basic test" $ do 
      1 `shouldBe` 1
  describe "day13" $ do
    it "should behave like the example" $ do 
      let input = "0: 3\n1: 2\n4: 4\n6: 4"
      let expected = 24
      day13 input `shouldBe` expected
