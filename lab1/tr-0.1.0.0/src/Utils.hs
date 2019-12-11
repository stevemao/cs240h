module Utils where

repeatFinalChar :: String -> String
repeatFinalChar as = case reverse as of
    (a : _) -> as ++ repeat a
    [] -> as
