module Day19Spec where

import Day19
import Test.Hspec

spec :: Spec
spec = do
    describe "Day19" $ do
        it "should answer the question according to the example" $ do
            let input = "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ "
            let expected = "ABCDEF"
            day19 input `shouldBe` expected
            
--    describe "traverseRoute" $ do
--        it "should "