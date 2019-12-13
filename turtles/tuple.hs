{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.Time

tuple :: Pattern (Int, Int)
tuple = do
    "("
    f <- decimal
    ","
    s <- decimal
    ")"
    return (f, s)
