module Day18Spec where

import Day18
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $
            1 `shouldBe` 1
    --describe "Day18" $ do
