module Day7Spec where
import Day7
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Day7" $ do
        it "should give a step count of 5"  $ do 
            let expected = "xhth"
            let input = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"
            day7 input `shouldBe` expected
