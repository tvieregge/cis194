module Main where

import Editor
import JoinList
import Lib
import Scrabble
import Sized

main = runEditor editor $ Single (Score 1, Size 1) "x"
