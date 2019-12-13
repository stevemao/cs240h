{-# LANGUAGE OverloadedStrings #-}

import Turtle

tuple :: Pattern (Int, Int)
tuple = do
    "("
    f <- decimal
    ","
    s <- decimal
    ")"
    return (f, s)
