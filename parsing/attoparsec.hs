{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative

request = (,,) <$>
          (verb <* skipSpace) <*>
          (url <* skipSpace) <*>
          (version <* endOfLine)

verb = "GET" <|> "POST"
url = takeTill A.isSpace
version = (,) <$> ("HTTP/" *> decimal) <*> ("." *> decimal)
