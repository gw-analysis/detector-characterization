


import HasKAL.ExternalUtils.LIGO.NDS2.Function ( Daq_channel_t (..)
                                               , getChannels
                                               , showChannelInfo
                                               )
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)


main :: IO ()
main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: ctl_showChannels [OPTION...] gps"
  let ip_nds = "10.68.10.122"
      port   = 8088
      gps    = read (head varArgs) :: Int
  selectedCHlist <- case selectKeys varOpt of
    Just skey ->
      do let skeys = words skey
             chlist = getChannels ip_nds port gps
         return $ selectKeywords skeys chlist
    Nothing ->
      return $ getChannels ip_nds port gps
  eliminatedCHlist <- case eliminateKeys varOpt of
    Just ekey -> do
      do let ekeys = words ekey
         return $ eliminateKeywords ekeys selectedCHlist
    Nothing -> return selectedCHlist
  mapM_ showChannelInfo eliminatedCHlist


selectKeywords :: [String]
               -> [Daq_channel_t]
               -> [Daq_channel_t]
selectKeywords as xs = filter (\x-> ch_name x `elem` as) xs


eliminateKeywords :: [String]
                  -> [Daq_channel_t]
                  -> [Daq_channel_t]
eliminateKeywords as xs = filter (\x-> ch_name x `notElem` as) xs


data Options = Options
  { selectKeys     :: Maybe String
  , eliminateKeys  :: Maybe String
  } deriving (Show)

defaultOptions = Options
 { selectKeys     = Nothing
 , eliminateKeys  = Nothing
 }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['s'] ["selectkeys"]
      ( ReqArg (\ f opts -> opts {selectKeys = Just f}) "SELECTKEYWORD")
      "set selectkeywords: keyword1 keyword2 ..."
  , Option ['e'] ["eliminatekeys"]
      ( ReqArg (\ f opts -> opts {eliminateKeys = Just f}) "ELIMINATEKEYWORD")
      "set eliminatekeywords: keyword1 keyword2 ..."
  ]
