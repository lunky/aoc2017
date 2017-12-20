module Day10Spec where
import Day10
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "twist'" $ do
    it "should perform simple twist [3,4,2,1,0]"  $ do 
      let count = 3
      let skipSize = 0
      let input = ([0..4],0,skipSize)
      let expected = ([3,4,2,1,0],3, skipSize+1)
      twist' input count `shouldBe` expected
    it "should perform simple twist [1,2,4,3,0]"  $ do 
      let count = 4
      let skipSize = 1
      let input = ([3,4,2,1,0],3,skipSize)
      let expected = ([1,2,4,3,0],3,skipSize+1)
      twist' input count `shouldBe` expected
    it "should perform simple twist [3,0,1,2,4]"  $ do 
      let count = 1
      let skipSize = 2
      let input = ([1,2,4,3,0],3,skipSize)
      let expected = ([3,0,1,2,4],1,skipSize+1)
      twist' input count `shouldBe` expected
    it "should perform simple twist [0,3,4,2,1]"  $ do 
      let count = 5
      let skipSize = 3
      let input = ([3,0,1,2,4],1,skipSize)
      let expected = ([0,3,4,2,1],4,skipSize+1)
      twist' input count `shouldBe` expected
    describe "9 Day10" $ do
      it "should perform simple twist"  $ do 
        let input = "3,4,1,5"
        let window = [0..4]
        let expected = 12
        day10 input window `shouldBe` expected