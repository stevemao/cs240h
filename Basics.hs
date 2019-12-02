module Basic where

data Move = Rock | Paper | Scissors
     deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

outcome :: Move -> Move -> Outcome
outcome Paper Rock = Win
outcome Scissors Paper = Win
outcome Rock Scissors = Win
outcome us them 
    | us == them = Tie
    | otherwise = Lose

parseMove :: String -> Maybe Move
parseMove str | [(m, rest)] <- reads str, ok rest = Just m
              | otherwise                         = Nothing
    where ok = all (`elem` " \r\n")
