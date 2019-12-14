type Param = (String, String)

type ContentType = String

data Payload = NoPayload
             | Raw ContentType String
             | Params [Param]
             | FormData [Part]
               deriving (Show)

instance Monoid Payload where
    mempty = NoPayload
    