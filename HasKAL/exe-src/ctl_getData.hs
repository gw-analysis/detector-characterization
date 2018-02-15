

import qualified Data.Vector.Storable as V
import HasKAL.ExternalUtils.LIGO.NDS2.Function ( Daq_channel_t (..)
                                               , getChannels
                                               , getData
                                               , selectKeywords
                                               , eliminateKeywords)
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
        where header = "Usage: ctl_getData [OPTION...] channel gps[localtime] duration"

  let ip_nds = "10.68.10.122"
      port   = 8088
      channel = head varArgs
      duration = read (varArgs!!2) :: Int
      gps = case setLocaltime varOpt of
              True  -> read (varArgs!!1) :: Int
              False -> read (time2gps (varArgs!!1)) :: Int
      chinfo = head $ selectKeywords [channel] $ getChannels ip_nds port gps
      fs = ch_rate chinfo :: Double
      maybev = getData ip_nds port [(channel,fs)] gps (gps+duration) duration
  case maybev of
    Nothing -> return ()
    Just v' -> do
      let v = head (head v')
      mapM_ (\x -> hPutStrLn stdout $ show x) (V.toList v)



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
      "ctl_getData -l channel \"2017-01-01 00:00:00 JST\" duration"
--  , Option ['e'] ["eliminatekeys"]
--      ( ReqArg (\ f opts -> opts {eliminateKeys = Just f}) "ELIMINATEKEYWORD")
--      "set eliminatekeywords: keyword1 keyword2 ..."
  ]
