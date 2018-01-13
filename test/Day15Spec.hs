module Day15Spec where
import Day15
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "test" $ do
    it "run a basic test" $ do 
      1 `shouldBe` 1
  describe "Day15" $ do
    it "should produce the judges final count" $ do
       let input = "Generator A starts with 65\nGenerator B starts with 8921"
       let count = 40000000
       let expected = 588
       day15 count input `shouldBe` expected
    it "should produce the correct result" $ do
      let input = 65
      let factor = 16807
      let expected = 1092455
      day15Function input factor `shouldBe` expected
    it "should produce the correct tuple" $ do
      let input = (65,8921)
      let expected = (1092455, 430625591)
      generator input `shouldBe` expected
  describe "Day15" $ do
    it "should produce the judges final count" $ do
       let input = "Generator A starts with 65\nGenerator B starts with 8921"
       let count = 5000000
       let expected = 309
       day15b count input `shouldBe` expected
