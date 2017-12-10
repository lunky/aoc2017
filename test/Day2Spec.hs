module Day2Spec (main, spec) where
import Day2
import Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $
            1 `shouldBe` 1
    describe "Day2" $ do
        it "should give a checksum"  $ do 
            let expected = 18
            let input = "5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8"
            day2 input `shouldBe` expected
    describe "Day2b" $ do
        it "should give a checksum"  $ do 
            let expected = 9
            let input = "5\t9\t2\t8\n9\t4\t7\t3\n3\t8\t6\t5"
            day2b input `shouldBe` expected
