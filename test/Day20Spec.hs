module Day20Spec where

import Day20
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $ do
            1 `shouldBe` 1
    --describe "Day20" $ do
