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
    it "should count find 2 connected lengths" $ do 
      let input = ["00100", "00100", "00110", "00100","00000"]
      let expected = 2
      (length $ twoDimensionGroup input) `shouldBe` expected
    it "should find single" $ do 
      let input = [((0,0),'0'),((0,1),'0'),((0,3),'0'),((0,4),'0'),
                   ((1,0),'0'),((1,1),'0'),((1,3),'0'),((1,4),'0'),
                   ((2,0),'0'),((2,1),'0'),((2,3),'0'),((2,4),'0'),
                   ((3,0),'0'),((3,1),'0'),((3,3),'0'),((3,4),'0'),
                   ((4,0),'0'),((4,1),'0'),((4,2),'0'),((4,3),'0'),((4,4),'0')]
      let expected = 1
      (length $ qgroup input) `shouldBe` expected
  
  it "should count one single group..." $ do 
      let input = [((0,0),'0'),            ((0,2),'0'),((0,3),'0'),((0,4),'0'),((0,5),'0'),
                   ((1,0),'0'),            ((1,2),'0'),                        ((1,5),'0'),
                   ((2,0),'0'),            ((2,2),'0'),((2,3),'0'),            ((2,5),'0'),
                   ((3,0),'0'),                                                ((3,5),'0'),
                   ((4,0),'0'),((4,1),'0'),((4,2),'0'),((4,3),'0'),((4,4),'0'),((4,5),'0')]
      let expected = 1
      (length $ qgroup input) `shouldBe` expected
  it "should count two groups..." $ do 
      let input = [((0,0),'0'),((0,1),'0'),((0,2),'0'),((0,3),'0'),((0,4),'1')]
      let expected = 2
      (length $ qgroup input) `shouldBe` expected
  it "should count two groups..." $ do 
      let input = [((0,0),'1'),((0,1),'0'),((0,2),'0'),((0,3),'0'),((0,4),'0')]
      let expected = 2
      (length $ qgroup input) `shouldBe` expected
  it "should count two groups..." $ do 
      let input = [((1,0),'0'),((2,0),'0'), ((1,2),'0'),((2,2),'0')]
      let expected = 2
      (length $ qgroup input) `shouldBe` expected
      
  it "should be 3 groups" $ do 
      let input = ["1111",
                 "0100",
                 "0100",
                 "0100"]
      let expected = 3
      (length $ twoDimensionGroup input) `shouldBe` expected
            

-- too slow to run
  describe "day14b" $ do
    it "should count regions" $ do 
      pendingWith "too slow to run"
      let input = "flqrgnkx"
      let expected =1242 
      day14b input `shouldBe` expected
