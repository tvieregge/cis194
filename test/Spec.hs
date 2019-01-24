import Lib

main :: IO ()
main = putStrLn . show $ hanoi 3 "a" "b" "c"
