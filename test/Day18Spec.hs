module Day18Spec where

import Day18
import Test.Hspec
import Data.Map

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $
            1 `shouldBe` 1
    --describe "Day18" $ do
    describe "parseSet" $ do
        it "should parse a Set" $ do
            let input = "set a 1"
            let expected = [Set 'a' 1]
            parseInput input `shouldBe` expected
        it "should parse a Set with a negative" $ do
            let input = "set a -1"
            let expected = [Set 'a' (-1)]
            parseInput input `shouldBe` expected
    describe "parseAdd" $ do
        it "should parse a Add" $ do
            let input = "add a 1"
            let expected = [Add 'a' 1]
            parseInput input `shouldBe` expected
    describe "parseMul" $ do
        it "should parse a Mul" $ do
            let input = "mul a 1"
            let expected = [Mul 'a' 1]
            parseInput input `shouldBe` expected
    describe "parseMod" $ do
        it "should parse a Mul" $ do
            let input = "mod a 1"
            let expected = [Mod 'a' 1]
            parseInput input `shouldBe` expected
    describe "parseJgz" $ do
        it "should parse a Jgz" $ do
            let input = "jgz a 1"
            let expected = [Jgz 'a' 1]
            parseInput input `shouldBe` expected
    describe "parseRcv" $ do
        it "should parse a Rcv" $ do
            let input = "rcv a"
            let expected = [Rcv 'a']
            parseInput input `shouldBe` expected
    describe "parseSnd" $ do
        it "should parse a Snd" $ do
            let input = "snd a"
            let expected = [Snd 'a']
            parseInput input `shouldBe` expected
    describe "doTheThing" $ do
        it "should set a register" $ do
            let input1 = Set 'a' 1
            let input2 = fromList []
            let expected = (fromList [('a',1::Int)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should set a register from another register" $ do
            let input1 = SetRef 'a' 'b' 
            let input2 = fromList [('b',3),('a',1)]
            let expected = (fromList [('b',3),('a',3)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should set a new register from another register" $ do
            let input1 = SetRef 'a' 'b' 
            let input2 = fromList [('b',3)]
            let expected = (fromList [('b',3),('a',3)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should set a new register from another register" $ do
            let input1 = SetRef 'a' 'b' 
            let input2 = fromList [('b',3)]
            let expected = (fromList [('b',3),('a',3)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should add a number to a register" $ do
            let input1 = Add 'a' 1
            let input2 = fromList [('a',3)]
            let expected = (fromList [('a',4)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should add a register to another register" $ do
            let input1 = AddRef 'a' 'b'
            let input2 = fromList [('a',3),('b',9)]
            let expected = (fromList [('a',12),('b',9)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should multiply a number to a register" $ do
            let input1 = Mul 'a' 3
            let input2 = fromList [('a',3)]
            let expected = (fromList [('a',9)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should multiply a number to an empty register" $ do
            let input1 = Mul 'a' 3
            let input2 = fromList []
            let expected = (fromList [],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should multiply a number to an empty register" $ do
            let input1 = MulRef 'a' 'b'
            let input2 = fromList []
            let expected = (fromList [],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        
        it "should add a register to another register" $ do
            let input1 = MulRef 'a' 'b'
            let input2 = fromList [('a',3),('b',9)]
            let expected = (fromList [('a',27),('b',9)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should mudulo a number to a register" $ do
            let input1 = Mod 'a' 9 
            let input2 = fromList [('a',3)]
            let expected = (fromList [('a',3)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should mudulo a number not found in registery" $ do
            let input1 = Mod 'a' 9 
            let input2 = fromList [('b',3)]
            let expected = (fromList [('b',3)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should mudulo a number not found in registery" $ do
            let input1 = ModRef 'a' 'd' 
            let input2 = fromList [('b',3)]
            let expected = (fromList [('b',3)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
        it "should modulo a register to another register" $ do
            let input1 = ModRef 'a' 'b'
            let input2 = fromList [('a',9),('b',5)]
            let expected = (fromList [('a',4),('b',5)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
            
        it "should play a value in a register" $ do
            let input1 = Snd 'a'
            let input2 = fromList [('a',9),('b',5)]
            let expected = (fromList [('a',9),('b',5),('=',9)],1)
            doTheThing ([],0) input1 input2 `shouldBe` expected
            
        it "should recieve a value in a register" $ do
            let input1 = Rcv 'a'
            let input2 = fromList [('a',1),('b',5),('=',9)]
            let expected = (fromList [('+',9),('a',1),('b',5),('=',9)],1)
            doTheThing ([],0) input1 input2  `shouldBe` expected
            
        it "shouldnt recieve a value in a register" $ do
            let input1 = Rcv 'a'
            let input2 = fromList [('a',4),('b',5),('=',1)]
            let expected = (fromList [('+',1),('a',4),('b',5),('=',1)],1)
            doTheThing ([],0) input1 input2  `shouldBe` expected
    describe "doAllTheThing" $ do
        it "shouldnt recieve a series of values" $ do
            let input = "set a 9\nset b 2\nmul a b\nadd a 2"
            let expected = fromList [('a', 20),('b',2)]
            (doAllTheThings $ parseInput input) `shouldBe` expected
        it "shouldnt recieve a series of values" $ do
            let input = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a" -- \nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
            let expected = 4
            (doAllTheThings $ parseInput input) ! '=' `shouldBe` expected
        it "shouldnt recieve a series of values" $ do
            let input = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
            let expected = 4
            (doAllTheThings $ parseInput input) ! '=' `shouldBe` expected
    describe "day18" $ do
        it "should return 4" $ do
            let input = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
            let expected = 4
            day18 input `shouldBe` expected
