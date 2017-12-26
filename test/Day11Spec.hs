module Day11Spec where
import Day11
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day11 move" $ do
    it "run a basic test" $ do 
      1 `shouldBe` 1
    it "move ne,ne,ne should be (3,0)" $ do 
      let input = "ne,ne,ne"
      let expected = (3,0)
      move (0,0) input `shouldBe` expected
    it "move ne,ne,sw,sw should be (0,0)" $ do 
      let input = "ne,ne,sw,sw"
      let expected = (0,0)
      move (0,0) input `shouldBe` expected
    it "move ne,ne,s,s should be (2,0)" $ do 
      let input = "ne,ne,s,s"
      let expected = (2,-2)
      move (0,0) input `shouldBe` expected
    it "move se,sw,se,sw,sw should be (-1,-5)" $ do 
      let input = "se,sw,se,sw,sw"
      let expected = (-1,1)
      move (0,3) input `shouldBe` expected
  describe "diag" $ do
    it "should give 3 for (3,3)" $ do
      let input1 = (0,0)
      let input2 = (3,0)
      let expected = 3
      diag input1 input2 `shouldBe` expected
      
    it "(0,0) shouldBe 0" $ do 
      let input1 = (0,0)
      let input2 = (0,0)
      let expected = 0
      diag input1 input2 `shouldBe` expected
      
    it "(0,2) shouldBe 2" $ do
      let input1 = (0,0)
      let input2 = (2,0)
      let expected = 2
      diag input1 input2 `shouldBe` expected
      
    it "(-2,0) shouldBe 2" $ do
      let input1 = (0,0)
      let input2 = (-2,0)
      let expected = 2
      diag input1 input2 `shouldBe` expected
    it "(-2,1) shouldBe 2" $ do
      let input1 = (0,0)
      let input2 = (-2,1)
      let expected = 1
      diag input1 input2 `shouldBe` expected
      
  describe "day11" $ do
    it "ne,ne,ne should be 3" $ do 
      let input = "ne,ne,ne"
      let expected = 3
      day11 input `shouldBe` expected
    it "move ne,ne,sw,sw should be 0" $ do 
      let input = "ne,ne,sw,sw"
      let expected = 0
      day11 input `shouldBe` expected
    it "move ne,ne,s,s should be 2" $ do 
      let input = "ne,ne,s,s"
      let expected = 2
      day11 input `shouldBe` expected
    it "move se,sw,se,sw,sw should be 3" $ do 
      let input = "se,sw,se,sw,sw"
      let expected = 3
      day11 input `shouldBe` expected
      