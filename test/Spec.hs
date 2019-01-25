import Lib

main :: IO ()
main = do
    putStrLn . show $ hanoi 3 "a" "b" "c"
    putStrLn . show $ parseMessage "ignored"
    putStrLn . show $ parseMessage "E"
