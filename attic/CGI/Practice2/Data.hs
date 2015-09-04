

module Data (
  Message
, ParamCGI(..)
, MultiSelect(..)
, updateMsg
, updateGps
) where

type Message = String
data MultiSelect = Single | Multi deriving Eq

data ParamCGI = ParamCGI
  { script :: String
  , message :: Message
  , files :: [String]
  , lstfile :: String
  , chlist :: [String]
  , gps :: Maybe String
  , locale :: String
  , channel1 :: [String]
  , channel2 :: [String]
  , monitors :: [String]
  , duration :: String
  , fmin :: String
  , fmax  :: String
  } deriving (Show, Eq, Read)

updateMsg :: Message -> ParamCGI -> ParamCGI
updateMsg msg params =
  ParamCGI { script = script params
           , message = msg
           , files = files params
           , lstfile = lstfile params
           , chlist = chlist params
           , gps = gps params
           , locale = locale params
           , channel1 = channel1 params
           , channel2 = channel2 params
           , monitors = monitors params
           , duration = duration params
           , fmin = fmin params
           , fmax = fmax params
           }

updateGps :: String -> ParamCGI -> ParamCGI
updateGps newGps params =
  ParamCGI { script = script params
           , message = message params
           , files = files params
           , lstfile = lstfile params
           , chlist = chlist params
           , gps = Just newGps
           , locale = locale params
           , channel1 = channel1 params
           , channel2 = channel2 params
           , monitors = monitors params
           , duration = duration params
           , fmin = fmin params
           , fmax = fmax params
           }


