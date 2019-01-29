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
