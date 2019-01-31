import ExprT
import Parser
import Lib
import Log
import Test.Hspec

main :: IO ()
main =
    hspec $ do
        describe "hanoi" $ do
            it "can solve the tower of hanoi problem" $ do
                let ans =
                        [ ("a", "c")
                        , ("a", "b")
                        , ("c", "b")
                        , ("a", "c")
                        , ("b", "a")
                        , ("b", "c")
                        , ("a", "c")
                        ]
                hanoi 3 "a" "b" "c" `shouldBe` ans
        describe "log parsing" $ do
            it "can ignore (some) bad input" $ do
                parseMessage "ignored" `shouldBe` Unknown "ignored"
            it "can parse all message types" $ do
                parseMessage "E 1 1 I'm an error" `shouldBe`
                    LogMessage (Error 1) 1 "I'm an error"
                parseMessage "W  1 I'm a warn" `shouldBe`
                    LogMessage Warning 1 "I'm a warn"
                parseMessage "I  1 I'm an info" `shouldBe`
                    LogMessage Info 1 "I'm an info"
            it "can read an input file" $ do
                let ans =
                        [ LogMessage Info 6 "6"
                        , LogMessage Info 1 "1"
                        , LogMessage Info 4 "4"
                        , LogMessage Info 11 "11"
                        , LogMessage (Error 70) 70 ""
                        ]
                testParse parse 5 "test/sample.log" `shouldReturn` ans
            it "can build a BST" $ do
                log <- testParse parse 5 "test/sample.log"
                let ans =
                        Node
                            (Node
                                 Leaf
                                 (LogMessage Info 1 "1")
                                 (Node Leaf (LogMessage Info 4 "4") Leaf))
                            (LogMessage Info 6 "6")
                            (Node
                                 Leaf
                                 (LogMessage Info 11 "11")
                                 (Node Leaf (LogMessage (Error 70) 70 "") Leaf))
                    inOrdAns =
                        [ LogMessage Info 1 "1"
                        , LogMessage Info 4 "4"
                        , LogMessage Info 6 "6"
                        , LogMessage Info 11 "11"
                        , LogMessage (Error 70) 70 ""
                        ]
                    tree = foldl (\acc x -> insert x acc) Leaf log
                tree `shouldBe` ans
                inOrder tree `shouldBe` inOrdAns
        describe "code golf!" $ do
            it "plays hopscotch... sorta" $ do
                skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
            it "finds local maxima" $ do
                localMaxima [2, 9, 5, 6, 1, 5] `shouldBe` [9, 6]
                localMaxima' [2, 9, 5, 6, 1, 5] `shouldBe` [9, 6]
        describe "week 4" $ do
            it "refactor in an idomatic way" $ do
                fun1' [3 .. 100] `shouldBe` fun1 [3 .. 100]
                fun2' 200 `shouldBe` fun2 200
            it "can build a balance binary tree with foldr" $
                -- terrible test, man valid trees
                -- replace with isBalanced + contains all elements (once)
             do
                let ans =
                        TNode
                            2
                            (TNode 1 (TNode 0 TLeaf 'A' TLeaf) 'C' TLeaf)
                            'D'
                            (TNode 0 TLeaf 'B' TLeaf)
                foldTree "ABCD" `shouldBe` ans
        describe "Week 5, calculator" $ do
            it "evaluates an expression" $ do
                eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
            it "evaluate a string" $ do
                evalStr "(2+3)*4" `shouldBe` Just 20
                evalStr "(2++3)*$" `shouldBe` Nothing
            it "can make ExprT an instance of Expr" $ do
                mul (add (lit 2) (lit 3)) (lit 4) `shouldBe`
                    Mul (Add (Lit 2) (Lit 3)) (Lit 4)
                let testExp = parseExp lit add mul "(3*-4)+5"
                (testExp :: Maybe Bool) `shouldBe` Just True
            it "calculates a stream of fib numbers in O(n)" $ do
                take 5 fibs2 `shouldBe` [0,1,1,2,3]
