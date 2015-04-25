module Data where

data Param = Param
  { sethome :: String
  , setsave :: String
  , setsaveLatest :: String
  , channellist :: [(String, Double)]
  , filetype :: String
  } deriving (Show, Eq, Read)

data ChanOutPut = ChanOutPut
  { inherit :: Param
  , gpstime :: String
  , dt :: String
  } deriving (Show, Eq, Read)

