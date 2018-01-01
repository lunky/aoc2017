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
--  describe "day14" $ do
--    it "should count squares" $ do 
--      let input = "jxqlasbh"
--      let expected = 8108
--      day14 input `shouldBe` expected
