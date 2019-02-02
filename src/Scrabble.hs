{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Map
import Data.Char

scores =
    fromList
        [ ('A', 1)
        , ('E', 1)
        , ('I', 1)
        , ('O', 1)
        , ('U', 1)
        , ('L', 1)
        , ('N', 1)
        , ('S', 1)
        , ('T', 1)
        , ('R', 1)
        , ('D', 2)
        , ('G', 2)
        , ('B', 3)
        , ('C', 3)
        , ('M', 3)
        , ('P', 3)
        , ('F', 4)
        , ('H', 4)
        , ('V', 4)
        , ('W', 4)
        , ('Y', 4)
        , ('K', 5)
        , ('J', 8)
        , ('X', 8)
        , ('Q', 10)
        , ('Z', 10)
        ]

newtype Score =
    Score Int
    deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = Score 0

score :: Char -> Score
score c = Score $ Data.Map.findWithDefault 0 (toUpper c) scores

scoreString :: String -> Score
scoreString = mconcat . Prelude.map score

