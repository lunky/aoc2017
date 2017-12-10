module Day6Spec where
import Day6
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day6" $ do
        it "should give a step count of 5"  $ do 
            let expected = 5 
            let input = "0\t2\t7\n0"
            day6 input `shouldBe` expected
--            [[0,2,7,0],[2,4,1,2],[3,1,2,3],[0,2,3,4],[1,3,4,1],[2,4,1,2]]
    describe "redistribute" $ do
        it "[0,2,7,0] should give next iteration"  $ do 
            let input = [0,2,7,0]
            let expected = [2,4,1,2]
            redistribute input `shouldBe` expected
        it "[2,4,1,2] should give next iteration"  $ do 
            let input = [2,4,1,2]
            let expected = [3,1,2,3]
            redistribute input `shouldBe` expected
        it "[3,1,2,3] should give next iteration"  $ do 
            let input = [3,1,2,3]
            let expected = [0,2,3,4]
            redistribute input `shouldBe` expected
        it "[0,2,3,4] should give next iteration"  $ do 
            let input = [0,2,3,4]
            let expected = [1,3,4,1]
            redistribute input `shouldBe` expected

    describe "Day6b" $ do
        it "should give a step count of 5"  $ do 
            let input = "0\t2\t7\n0"
            let expected = 4 
            day6b input `shouldBe` expected