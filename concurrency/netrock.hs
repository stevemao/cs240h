module Main where
import Control.Concurrent
import Control.Exception
import Network
import System.IO

data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
                | otherwise  = Lose

parseMove :: String -> Maybe Move
parseMove str = case reads str of
  [(m, rest)] | ok rest -> Just m
  _                     -> Nothing
  where ok = all (`elem` " \r\n")

getMove :: Handle -> IO Move
getMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
  input <- hGetLine h
  case parseMove input of Just move -> return move
                          Nothing -> getMove h

computerVsUser :: Move -> Handle -> IO ()
computerVsUser computerMove h = do
  userMove <- getMove h
  let o = outcome userMove computerMove
  hPutStrLn h $ "You " ++ show o

withTty :: (Handle -> IO a) -> IO a
withTty = withFile "/dev/tty" ReadWriteMode

withClient :: PortID -> (Handle -> IO a) -> IO a
withClient listenPort fn =
  bracket (listenOn listenPort) sClose $ \s -> do
    bracket (accept s) (\(h, _, _) -> hClose h) $
      \(h, host, port) -> do
        putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
        fn h

-- You may find defining this function useful
play :: MVar Move -> MVar Move -> (Handle, HostName, PortNumber) -> IO ()
play myMoveMVar opponentMoveMVar (h, host, port) = undefined

-- You should define this function
netrock :: PortID -> IO ()
netrock listenPort = undefined

main :: IO ()
main = netrock (PortNumber 1617)
