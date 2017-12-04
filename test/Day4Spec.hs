module Day4Spec where
import Day4
import Lib
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day4" $ do
        it "should give a True response"  $ do 
            let expected = True
            let input = "aa bb cc dd ee"
            day4' input `shouldBe` expected
        it "should give a False response"  $ do 
            let expected = False 
            let input = "aa bb cc dd aa"
            day4' input `shouldBe` expected
        it "should give a True response"  $ do 
            let expected = True 
            let input = "aa bb cc dd aaa"
            day4' input `shouldBe` expected
    describe "Day4b" $ do
        it "should give a True response"  $ do 
            let expected = True 
            let input = "abcde fghij"
            day4b' input `shouldBe` expected
        it "should give a True response"  $ do 
            let expected = False
            let input = "abcde xyz ecdab"
            day4b' input `shouldBe` expected