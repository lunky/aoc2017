module Day9Spec where
import Day9
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day9" $ do
        it "should parse single"  $ do 
            let input = "{}"
            let expected = 1
            day9 input `shouldBe` expected
        it "should parse triple"  $ do 
            let input = "{{{}}}"
            let expected = 6
            day9 input `shouldBe` expected
        it "should parse nested"  $ do 
            let input = "{{},{}}"
            let expected = 5
            day9 input `shouldBe` expected
        it "should parse nested"  $ do 
            let input = "{{{},{},{{}}}}"
            let expected = 16
            day9 input `shouldBe` expected
        it "should parse nested"  $ do 
            let input = "{<a>,<a>,<a>,<a>}"
            let expected = 1
            day9 input `shouldBe` expected
        it "should parse nested"  $ do 
            let input = "{{<ab>},{<ab>},{<ab>},{<ab>}}"
            let expected = 9
            day9 input `shouldBe` expected
        it "should parse nested"  $ do 
            let input = "{{<!!>},{<!!>},{<!!>},{<!!>}}"
            let expected = 9
            day9 input `shouldBe` expected
        it "should parse nested"  $ do 
            let input = "{{<a!>},{<a!>},{<a!>},{<ab>}}"
            let expected = 3
            day9 input `shouldBe` expected
    describe "Day9b" $ do
        it "should parse empty"  $ do 
            let input = "<>"
            let expected = 0
            day9b input `shouldBe` expected
        it "should parse chars"  $ do 
            let input = "<random characters>"
            let expected = 17
            day9b input `shouldBe` expected
        it "should parse unbalanced"  $ do 
            let input = "<<<<>"
            let expected = 3
            day9b input `shouldBe` expected
        it "should parse first > is canceled."  $ do 
            let input = "<{!>}>"
            let expected = 2
            day9b input `shouldBe` expected
        it "should parse double !"  $ do 
            let input = "<!!>"
            let expected = 0
            day9b input `shouldBe` expected
        it "should parse triple bang"  $ do 
            let input = "<!!!>>"
            let expected = 0
            day9b input `shouldBe` expected
        it "should parse escaped"  $ do 
            let input = "<{o\"i!a,<{i<a>"
            let expected = 10
            day9b input `shouldBe` expected