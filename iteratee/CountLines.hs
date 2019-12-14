import Control.Exception
import Control.Monad
import qualified Data.ByteString.Strict as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.FilePath
import System.Posix
import System.IO.Unsafe    -- for understanding, not recommended

readFiles :: [FilePath] -> IO L.ByteString
readFiles [] = return L.empty
readFiles (f:fs) = liftM2 L.append (L.readFile f)
                   (readFiles fs)

countLines :: FilePath -> IO ()
countLines dir =
    recDir dir >>= readFiles >>= print . L8.count '\n'

