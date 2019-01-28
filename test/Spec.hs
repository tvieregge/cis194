import Lib
import Log

main :: IO ()
main = do
    putStrLn . show $ hanoi 3 "a" "b" "c"
    putStrLn . show $ parseMessage "ignored"
    putStrLn . show $ parseMessage "E 1 1 I'm an error"
    putStrLn . show $ parseMessage "W  1 I'm a warn"
    putStrLn . show $ parseMessage "I  1 I'm an info"
    log <- testParse parse 5 "test/error.log"
    putStrLn $ show log
    putStrLn . show $ foldl (\acc x -> insert x acc) Leaf log
