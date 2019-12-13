{-# LANGUAGE OverloadedStrings #-}

import Turtle

-- inproc "nl" ["example.hs"] "" & output "numbered.txt"
a :: IO ()
a = input "example.hs" & inproc "nl" [] & output "numbered.txt"
