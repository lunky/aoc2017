module Day8Spec where
import Day8
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day8" $ do
        it "should calculate the value of the greatest register"  $ do 
            let input = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"
            let expected = 1
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let input = "b inc 1 if a < 1\n"
            let expected = 1
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let input = "b inc 1 if a == 1\n"
            let expected = 0
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let input = "b inc 1 if a >= 1\n"
            let expected = 0
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let input = "b inc 1 if a > 1\n"
            let expected = 0
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let input = "b inc 1 if a != 1\n"
            let expected = 1
            day8 input `shouldBe` expected


        it "should calculate the value of the greatest register"  $ do 
            let input = "b dec 1 if a < 1\n"
            let expected = (-1)
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let input = "b dec 1 if a == 1\n"
            let expected = 0
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let input = "b dec 1 if a >= 1\n"
            let expected = 0
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let input = "b dec 1 if a > 1\n"
            let expected = 0
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let expected = (-189)
            let input = "ehw inc -34 if ubu != -3\nehw inc -155 if oj <= 386"
            day8 input `shouldBe` expected
        it "should calculate the value of the greatest register"  $ do 
            let input = "it inc 355 if y <= 5"
            let expected = (355)
            day8 input `shouldBe` expected