module Utils where

repeatFinalChar :: Int -> String -> String
repeatFinalChar n as = case reverse as of
    (a : _) -> as ++ replicate n a
    [] -> as
