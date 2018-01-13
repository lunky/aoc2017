module Day16Spec where

import Day16
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $
            1 `shouldBe` 1
    --describe "Day16" $ do
