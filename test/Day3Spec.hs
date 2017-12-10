module Day3Spec where
import Day3
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day3" $ do
        it "should give a response"  $ do 
            let expected = 0
            let input = 1
            day3 input `shouldBe` expected
        it "should give a response 12 3"  $ do 
            let expected = 3
            let input = 12
            day3 input `shouldBe` expected
    describe "Day3b" $ do
        it "should give a response 1 1"  $ do 
            let expected = 1
            let input = 1
            day3b input `shouldBe` expected
        it "should give a response 2 1"  $ do 
            let expected = 1
            let input = 2
            day3b input `shouldBe` expected
        it "should give a response 3 2"  $ do 
            let expected = 2
            let input = 3
            day3b input `shouldBe` expected
        it "should give a response 4 4"  $ do 
            let expected = 4
            let input = 4
            day3b input `shouldBe` expected
            
        