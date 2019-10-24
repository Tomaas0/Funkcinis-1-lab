module Entities where
    data Msg = Empty | Msg { coord :: (String, String)
                            , result :: String
                            , prev :: Msg } deriving Show

