

import HasKAL.ExternalUtils.LIGO.NDS2.Function ( Daq_channel_t (..)
                                               , getChannels
                                               , showChannelInfo
                                               , selectKeywords)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)


main :: IO ()
main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: ctl_getChannelInfo [OPTION...] gps[localtime] channel1 channel2 ..."

  let ip_nds = "10.68.10.122"
      port   = 8088
      gps = case setLocaltime varOpt of
              True  -> read (head varArgs) :: Int
              False -> read (time2gps (head varArgs)) :: Int
  mapM_ showChannelInfo $ selectKeywords varArgs $ getChannels ip_nds port gps


data Options = Options
  { setLocaltime     :: Bool
--  ,
  } deriving (Show)

defaultOptions = Options
 { setLocaltime   = False
 }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['l'] ["localtime"]
      ( NoArg (\ opts -> opts {setLocaltime = True}))
      "ctl_getChannelInfo -l \"2017-01-01-00:00:00 JST\" channel"
--  , Option ['e'] ["eliminatekeys"]
--      ( ReqArg (\ f opts -> opts {eliminateKeys = Just f}) "ELIMINATEKEYWORD")
--      "set eliminatekeywords: keyword1 keyword2 ..."
  ]
