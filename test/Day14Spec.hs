module Day14Spec where
import Day14
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "test" $ do
    it "run a basic test" $ do 
      1 `shouldBe` 1
  describe "day14" $ do
    it "should count squares" $ do 
      let input = "flqrgnkx"
      let expected = 8108
      day14 input `shouldBe` expected
  describe "twoDimensionGroup" $ do
    it "should count matching horizontally adjacent " $ do 
      let input = ["aac","bdf","ghi"]
      let expected = 8
      (length $ twoDimensionGroup input) `shouldBe` expected
    it "should count matching vertically adjacent " $ do 
      let input = ["abc","adf","ghi"]
      let expected = 8
      (length $ twoDimensionGroup input) `shouldBe` expected
    it "should count matching vertically connected" $ do 
      let input = ["abc","adf","ahi"]
      let expected = 7
      (length $ twoDimensionGroup input) `shouldBe` expected
    it "should count matching not vertically connected" $ do 
      let input = ["abcj","adfk","lhim","anop"]
      let expected = 15
      (length $ twoDimensionGroup input) `shouldBe` expected
    it "should count matching not horizontally connected" $ do 
      let input = ["abaa","vdfk","whim","xnop"]
      let expected = 15
      (length $ twoDimensionGroup input) `shouldBe` expected
  
--  describe "day14b" $ do
--    it "should count regions" $ do 
--      let input = "flqrgnkx"
--      let expected =1242 
--      day14b input `shouldBe` expected
