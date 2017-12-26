module Day12Spec where
import Day12
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day12 move" $ do
    it "run a basic test" $ do 
      1 `shouldBe` 1
