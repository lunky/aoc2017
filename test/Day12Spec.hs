module Day12Spec where
import Day12
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day12" $ do
    it "run a basic test" $ do 
      1 `shouldBe` 1
  describe "day12" $ do
    it "should behave like the example" $ do 
      let input = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5\n"
      let expected = 6
      day12 input `shouldBe` expected
  describe "day12b" $ do
    it "should behave like the example" $ do 
      let input = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5\n"
      let expected = 2
      day12b input `shouldBe` expected
