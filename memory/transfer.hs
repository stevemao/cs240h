import Control.Concurrent
import Control.Monad

type Account = MVar Double

printMV :: (Show a) => MVar a -> IO ()
printMV mv = withMVar mv print

transfer :: Double -> Account -> Account -> IO ()
transfer amount from to = modifyMVar_ from (pure . (-) amount) >> putMVar to amount

main :: IO ()
main = do
  ac1 <- newMVar 10
  ac2 <- newMVar 0
  transfer 1 ac1 ac2
  printMV ac1
  printMV ac2
  return ()
