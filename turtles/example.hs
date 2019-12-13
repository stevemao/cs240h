{-# LANGUAGE OverloadedStrings #-}

import Turtle

run :: IO ()
run = inproc "nl" ["example.hs"] "" & output "numbered.txt"
