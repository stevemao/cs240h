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
