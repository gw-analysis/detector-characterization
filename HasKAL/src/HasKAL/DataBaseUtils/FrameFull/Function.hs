{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module HasKAL.DataBaseUtils.FrameFull.Function
( cleanDataFinder
, db2framelist
, db2framecache
, kagraChannelList
, kagraDailyFileList
, kagraDataFind
, kagraDataFind'
, kagraDataGet
, kagraDataGet'
, kagraDataGetC
, kagraDataGet0
, kagraDataGPS
, kagraDataPoint
, kagraDataFindCore
, kagraDataPointCore
, kagraWaveDataGet
, kagraWaveDataGetC
, kagraWaveDataGet0
)
where


import Control.Applicative ((<$>), Applicative(pure, (<*>)))
import Control.Monad
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Data.Int                   (Int32)
import Data.List                  (isInfixOf)
import Data.Maybe                 (fromJust, fromMaybe, catMaybes)
import qualified Data.Packed.Vector as DPV
import qualified Data.Traversable as DT
import qualified Data.Vector.Storable as V
import Database.Relational.Query ( relationalQuery
                                 , query
                                 , relation
                                 , wheres
                                 , not'
                                 , and'
                                 , or'
                                 , value
                                 , Relation
                                 , (.>=.)
                                 , (.<=.)
                                 , (.=.)
                                 , (!)
                                 , (|*|)
                                 , (|$|)
                                 )
import Database.HDBC.Session     (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC as DH
import Database.HDBC              (quickQuery', runRaw, fromSql, SqlValue)
import Database.Relational.Query.Pure (ProductConstructor,  productConstructor)
import Database.Record
import Database.Record.ToSql
import Foreign.C.Types
import HasKAL.DataBaseUtils.KAGRADataSource (connect)
import HasKAL.DataBaseUtils.FrameFull.Data
import HasKAL.DataBaseUtils.FrameFull.Table (Framefull(..), insertFramefull)
import qualified HasKAL.DataBaseUtils.FrameFull.Table as FrameFull
import qualified HasKAL.DetectorUtils.Detector as D
import HasKAL.FrameUtils.FrameUtils
import HasKAL.SearchUtils.Common.CleanDataFinder (cleanDataFinderCore)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature (GPSTIME, Date, LocalTime)
import HasKAL.TimeUtils.Function (formatGPS, deformatGPS)
import HasKAL.WaveUtils.Data (WaveData(..), mkWaveData, dropWaveData, takeWaveData)
import HasKAL.WaveUtils.Function (catWaveData, catWaveData0)
import System.IO.Unsafe (unsafePerformIO)


cleanDataFinder :: CDFParam -> String -> (GPSTIME, Double) -> IO (Maybe [(GPSTIME, Bool)])
cleanDataFinder param ch (gps', dt') = do
  let fs = cdf'samplingFrequency param
      fl = cdf'cutoffFrequencyLow param
      fu = cdf'cutoffFrequencyHigh param
      blcksz = cdf'blockSize param
      nfft = cdf'fftSize param
      nchunk = cdf'chunkSize param
      gpsstrt = floor $ deformatGPS gps' - dt'
      dt = floor dt' :: Int
  kagraDataGetC gpsstrt dt ch >>= \maybedat ->
    case maybedat of
      Nothing -> return Nothing
      Just timendat -> return $ Just $ flip concatMap timendat $ \x ->
        cleanDataFinderCore blcksz nfft nchunk (fl, fu) fs x


kagraChannelList :: Int32 -> IO (Maybe [String])
kagraChannelList gpstime = runMaybeT $ MaybeT $ do
  file <- kagraDataGPS gpstime >>= \maybel ->
    case maybel of
      Nothing -> print "no file in this GPS time." >> return Nothing
      Just xs  -> return $ Just (head xs)
  getChannelList (fromJust file) >>= \maybech ->
     case maybech of
       Nothing -> print "no channel in this GPS time." >> return Nothing
       Just xs -> return $ Just (fst . unzip $ xs)


kagraDailyFileList :: Date -> LocalTime -> IO (Maybe [String])
kagraDailyFileList day loc = runMaybeT $ MaybeT $ do
  if (length day) /= 10
    then error "Usage: kagraDailyFileList yyyy-mm-dd localtime"
    else do
      let day' = day++" 00:00:00 "++loc
          gpsstrt = read (time2gps day') :: Int32
          duration = 24*60*60 :: Int32
      flist <- kagraDataFindCore gpsstrt duration
      let out = [ u
                | (Just u) <- flist
                ]
      case out of
        []     -> return Nothing
        x -> return (Just x)


kagraDataFind :: Int32 -> Int32 -> String -> IO (Maybe [String])
kagraDataFind gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFindCore gpsstrt duration
  let out = [ u
            | (Just u) <- flist, (not . null) $ existChannel chname u
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


kagraDataFind' :: Int32 -> Int32 -> String -> IO (Maybe [((Int, Int), String)])
kagraDataFind' gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFindCore' gpsstrt duration
  let out = [ ((fromIntegral ts :: Int, fromIntegral te :: Int), u)
            | (Just ts, Just te, Just u) <- flist, (not . null) $ existChannel chname u
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


kagraDataPoint :: Int32 -> String -> IO (Maybe [String])
kagraDataPoint gpstime chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataPointCore gpstime
  let out = [ u
            | (Just u) <- flist, (not . null) $ existChannel chname u
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


existChannel chname fname = unsafePerformIO $ do
  getChannelList fname >>= \maybech -> case maybech of
    Nothing -> return []
    Just x -> let judge = filter (\ch -> ch == chname) $ (fst . unzip) x
               in case null judge of
                    True -> return []
                    False-> return fname


kagraDataGPS :: Int32 -> IO (Maybe [String])
kagraDataGPS gpstime = runMaybeT $ MaybeT $ do
  flist <- kagraDataGPSCore gpstime
  let out = [ u
            | (Just u) <- flist
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


kagraDataGet :: Int -> Int -> String -> IO (Maybe (V.Vector Double))
kagraDataGet gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFind (fromIntegral gpsstrt) (fromIntegral duration) chname
  case flist of
    Nothing -> return Nothing
    Just x -> do
      let headfile = head x
      getSamplingFrequency headfile chname >>= \maybefs ->
        case maybefs of
          Nothing -> return Nothing
          Just fs ->
            getGPSTime headfile >>= \maybegps ->
              case maybegps of
                Nothing -> return Nothing
                Just (gpstimeSec, gpstimeNano, dt) -> do
                  let headNum = if (fromIntegral gpsstrt - gpstimeSec) <= 0
                                  then 0
                                  else floor $ fromIntegral (fromIntegral gpsstrt - gpstimeSec) * fs
                      nduration = floor $ fromIntegral duration * fs
                  DT.sequence $ Just $ liftM (V.force . (V.slice headNum nduration) . V.concat)
                    $ forM x (\y -> do
                        maybex <- readFrameV chname y
                        return $ fromJust maybex)


kagraWaveDataGet :: Int -> Int -> String -> IO (Maybe WaveData)
kagraWaveDataGet gpsstrt duration chname = liftM (liftM catWaveData) $ kagraWaveDataGetC gpsstrt duration chname


kagraDataGet' :: Int -> Int -> String -> IO (Maybe (V.Vector CDouble))
kagraDataGet' gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFind (fromIntegral gpsstrt) (fromIntegral duration) chname
  case flist of
    Nothing -> return Nothing
    Just x -> do
      let headfile = head x
      getSamplingFrequency headfile chname >>= \maybefs ->
        case maybefs of
          Nothing -> return Nothing
          Just fs ->
            getGPSTime headfile >>= \maybegps ->
              case maybegps of
                Nothing -> return Nothing
                Just (gpstimeSec, gpstimeNano, dt) -> do
                  let headNum = if (fromIntegral gpsstrt - gpstimeSec) <= 0
                                  then 0
                                  else floor $ fromIntegral (fromIntegral gpsstrt - gpstimeSec) * fs
                      nduration = floor $ fromIntegral duration * fs
                  DT.sequence $ Just $ liftM (V.force . (V.slice headNum nduration) . V.concat)
                    $ forM x (\y -> do
                        maybex <- readFrameVCD chname y
                        return $ fromJust maybex)


kagraDataGetC :: Int -> Int -> String -> IO (Maybe [(GPSTIME,V.Vector Double)])
kagraDataGetC gpsstrt duration chname = runMaybeT $ MaybeT $ do
  tf <- kagraDataFind' (fromIntegral gpsstrt) (fromIntegral duration) chname
  case tf of
    Nothing -> return Nothing
    Just x -> do
      let headfile = snd . head $ x
      getSamplingFrequency headfile chname >>= \maybefs ->
        case maybefs of
          Nothing -> return Nothing
          Just fs ->
            getGPSTime headfile >>= \maybegps ->
              case maybegps of
                Nothing -> return Nothing
                Just (gpstimeSec, gpstimeNano, dt) -> do
                  let nInd = nConsecutive $ (fst . unzip) x
                      out' = consecutive (readFrameV chname) x nInd
                      gpsstop = fromIntegral $ gpsstrt + duration
                      gpsstrt' = fromIntegral gpsstrt
                  return $ Just $ map (checkStartGPS gpsstrt' fs . checkStopGPS gpsstop fs) out'


kagraWaveDataGetC :: Int -> Int -> String -> IO (Maybe [WaveData])
kagraWaveDataGetC gpsstrt duration chname = runMaybeT $ MaybeT $ do
  tf <- kagraDataFind' (fromIntegral gpsstrt) (fromIntegral duration) chname
  case tf of
    Nothing -> return Nothing
    Just x -> do
      let headfile = snd . head $ x
      getSamplingFrequency headfile chname >>= \maybefs ->
        case maybefs of
          Nothing -> return Nothing
          Just fs ->
            getGPSTime headfile >>= \maybegps ->
              case maybegps of
                Nothing -> return Nothing
                Just (gpstimeSec, gpstimeNano, dt) -> do
                  let nduration = floor $ fromIntegral duration * fs
                      nInd = nConsecutive $ (fst . unzip) x
                      timendat = consecutive (readFrameV chname) x nInd
                      gpsstop = fromIntegral gpsstrt + fromIntegral duration
                  return $ Just $ for timendat $ \y->
                    let ts = (fst $ fst y, 0)
                        xvec = snd y
                        nlen = V.length xvec
                        te' = deformatGPS ts + fromIntegral nlen/fs
                        te = formatGPS te'
                        element =  mkWaveData D.General chname fs ts te xvec
                        headNum = checkStartGPSW (fromIntegral gpsstrt) element
                        endNum = checkStopGPSW (fromIntegral gpsstop) element
                     in dropWaveData headNum $ takeWaveData (nlen-endNum) element


kagraDataGet0 :: Int -> Int -> String -> IO (Maybe (GPSTIME,V.Vector Double))
kagraDataGet0 gpsstrt duration chname = runMaybeT $ MaybeT $ do
  tf <- kagraDataFind' (fromIntegral gpsstrt) (fromIntegral duration) chname
  case tf of
    Nothing -> return Nothing
    Just x -> do
      let headfile = snd . head $ x
      getSamplingFrequency headfile chname >>= \maybefs ->
        case maybefs of
          Nothing -> return Nothing
          Just fs ->
            getGPSTime headfile >>= \maybegps ->
              case maybegps of
                Nothing -> return Nothing
                Just (gpstimeSec, gpstimeNano, dt) -> do
                  let headNum = if (fromIntegral gpsstrt - gpstimeSec) <= 0
                                  then 0
                                  else floor $ fromIntegral (fromIntegral gpsstrt - gpstimeSec) * fs
                      nduration = floor $ fromIntegral duration * fs
                      nInd = nConsecutive $ (fst . unzip) x
                      cdata = consecutive (readFrameV chname) x nInd
                  case length cdata of
                    0 -> return Nothing
                    1 -> do let gpsstop = fromIntegral $ gpsstrt + duration
                                gpsstrt' = fromIntegral gpsstrt
                            return $ Just $ (checkStartGPS gpsstrt' fs . checkStopGPS gpsstop fs) $ head cdata
                    _ -> do let gpsstop = fromIntegral $ gpsstrt + duration
                                gpsstrt' = fromIntegral gpsstrt
                            return $ Just $ (checkStartGPS gpsstrt' fs . checkStopGPS gpsstop fs) 
                              $ zeropadding fs cdata


kagraWaveDataGet0 :: Int -> Int -> String -> IO (Maybe WaveData)
kagraWaveDataGet0 gpsstrt duration chname = liftM (liftM (catWaveData0 0)) $ kagraWaveDataGetC gpsstrt duration chname



checkStopGPS :: Double -> Double -> (GPSTIME, V.Vector Double)-> (GPSTIME, V.Vector Double)
checkStopGPS t0 fs wav = let (t,v) = wav
                             t1 = deformatGPS t
                          in if (t0 - t1) <= 0
                               then let ntake = V.length v - floor ((t1 - t0) * fs)
                                     in (t, V.take ntake v)
                               else wav


checkStartGPS :: Double -> Double -> (GPSTIME,V.Vector Double) -> (GPSTIME, V.Vector Double)
checkStartGPS t0 fs wav = let (t,v) = wav
                              t1 = deformatGPS t
                           in if (t0 - t1) <= 0
                              then wav
                              else let ndrop = floor $ (t0 - t1) * fs
                                    in (formatGPS t0, V.drop ndrop v)


checkStopGPSW t0 wav = let t1 = deformatGPS $ stopGPSTime wav :: Double
                        in if (t0 - t1) <= 0
                              then floor $ (t1-t0)*samplingFrequency wav
                              else 0


checkStartGPSW t0 wav = let t1 = deformatGPS $ startGPSTime wav :: Double
                         in if (t0 - t1) <= 0
                               then 0
                               else floor $ (t0 - t1) * samplingFrequency wav


zeropadding :: Double -> [(GPSTIME, V.Vector Double)] -> (GPSTIME, V.Vector Double)
zeropadding fs gpsnv =
  let x = [(deformatGPS gps, deformatGPS gps + fromIntegral (V.length v)/fs)|(gps, v)<-gpsnv]
      nx= length x
      n0pad = flip map [1, 2..(nx-1)] $ \i-> floor $ (fst (x!!i) - snd (x!!(i-1)))*fs
      v0pad = flip map n0pad $ \x-> V.fromList (replicate x 0.0)
      gpsnv' = (snd $ gpsnv!!0) : (flip map [1, 2..(nx-1)] $ \i -> V.drop (negate (n0pad!!(i-1))) (snd $ gpsnv!!i))
      zeroPaddedV = V.concat $ interleave gpsnv' v0pad
   in (formatGPS (fst (head x)), zeroPaddedV)


interleave [] ys = ys
interleave (x:xs) ys = x:interleave ys xs


consecutive :: (String -> IO (Maybe (V.Vector Double)))
            -> [((Int,Int),String)]
            -> [Int]
            -> [((Int,Int),V.Vector Double)]
consecutive f [] _ = []
consecutive f _ [] = []
consecutive f x (j:js) = do
  let y = take j x
      (tlist, flist) = unzip y
      v = V.concat $ for flist (fromJust . unsafePerformIO . f)
   in (((fst . head) tlist, 0), v) : consecutive f (drop j x) js


findConsecutiveInd :: [(Int,Int)] -> [Int]
findConsecutiveInd x =
  let (tss,tes) = unzip x
      y = zip (zip (tail tss) (init tes)) [1..]
      judge = filter (\(x,y) -> fst x /= snd x) y
   in case null judge of
        False -> snd . unzip $ judge
        True  -> [0]


nConsecutive :: [(Int,Int)] -> [Int]
nConsecutive x =
  let ind = findConsecutiveInd x
   in case (ind==[0]) of
        True -> [length x]
        False-> head ind : zipWith (-) (tail ind) (init ind) ++ [length x - last ind]


for = flip map


kagraDataFindCore :: Int32 -> Int32 -> IO [Maybe String]
kagraDataFindCore gpsstrt duration =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    gpsend = gpsstrt + duration
    flist = relation
      [ u
      | u <- query FrameFull.framefull
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query flist
      wheres $ not' ((ch ! FrameFull.gpsStart' .<=. value (Just gpsstrt)
        `and'` ch ! FrameFull.gpsStop'  .<=. value (Just gpsstrt))
        `or'` (ch ! FrameFull.gpsStart' .>=. value (Just gpsend)
        `and'` ch ! FrameFull.gpsStop'  .>=. value (Just gpsend)))
      return $ ch ! FrameFull.fname'


kagraDataFindCore' :: Int32 -> Int32 -> IO [(Maybe Int32, Maybe Int32, Maybe String)]
kagraDataFindCore' gpsstrt duration =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    gpsend = gpsstrt + duration
    flist = relation
      [ u
      | u <- query FrameFull.framefull
      ]
    core :: Relation () (Maybe Int32, Maybe Int32, Maybe String)
    core = relation $ do
      ch <- query flist
      wheres $ not' ((ch ! FrameFull.gpsStart' .<=. value (Just gpsstrt)
        `and'` ch ! FrameFull.gpsStop'  .<=. value (Just gpsstrt))
        `or'` (ch ! FrameFull.gpsStart' .>=. value (Just gpsend)
        `and'` ch ! FrameFull.gpsStop'  .>=. value (Just gpsend)))
      return $ (,,) |$| ch ! FrameFull.gpsStart' |*| ch ! FrameFull.gpsStop' |*| ch ! FrameFull.fname'


kagraDataPointCore :: Int32 -> IO [Maybe String]
kagraDataPointCore gpstime =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    channel = relation
      [ u
      | u <- query FrameFull.framefull
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ ch ! FrameFull.gpsStart' .<=. value (Just gpstime)
      wheres $ ch ! FrameFull.gpsStop'  .>=. value (Just gpstime)
      return $ ch ! FrameFull.fname'


kagraDataGPSCore :: Int32 -> IO [Maybe String]
kagraDataGPSCore gpstime =
  handleSqlError' $ withConnectionIO connect $ \conn ->
--  setSqlMode conn
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    channel = relation
      [ u
      | u <- query FrameFull.framefull
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ ch ! FrameFull.gpsStart' .<=. value (Just gpstime)
      wheres $ ch ! FrameFull.gpsStop'  .>=. value (Just gpstime)
      return $ ch ! FrameFull.fname'



db2framecache :: Relation () FrameFull.Framefull -> IO (Maybe [String])
db2framecache dbname = do
  maybefrlist <- db2framelist dbname
  case catMaybes maybefrlist of
    [] -> return Nothing
    x  -> return (Just x)


db2framelist :: Relation () FrameFull.Framefull -> IO [Maybe String]
db2framelist dbname =
  handleSqlError' $ withConnectionIO connect $ \ conn ->
  runQuery' conn (relationalQuery core) ()
    where
      core = relation $ do
        lists <- query dbname
        return $ lists ! FrameFull.fname'


setSqlMode conn = do
  mode <- quickQuery' conn "SELECT @@SESSION.sql_mode" []
  newmode <- case mode of
    [[sqlval]] ->
      let val = DH.fromSql sqlval in
        if "IGNORE_SPACE" `isInfixOf` val
          then return val
          else return $ val ++ ", IGNORE_SPACE"
    _          ->
      error "failed to get 'sql_mode'"
  runRaw conn $ "SET SESSION sql_mode = '" ++ newmode ++ "'"


instance ProductConstructor (a -> b -> c -> (a, b, c)) where
  productConstructor = (,,)

instance (FromSql SqlValue a, FromSql SqlValue b, FromSql SqlValue c)
         => FromSql SqlValue (a, b, c) where
  recordFromSql = (,,) <$> recordFromSql <*> recordFromSql <*> recordFromSql

instance (ToSql SqlValue a, ToSql SqlValue b, ToSql SqlValue c)
         => ToSql SqlValue (a, b, c) where
  recordToSql = createRecordToSql (\(a, b, c) -> fromRecord a ++ fromRecord b ++ fromRecord c)




