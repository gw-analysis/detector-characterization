{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module XEndEnvFunction
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

import Debug.Trace(trace)
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
import HasKAL.DataBaseUtils.XEndEnv.Data
import HasKAL.DataBaseUtils.XEndEnv.Table (Xendenv(..), insertXendenv)
import qualified HasKAL.DataBaseUtils.XEndEnv.Table as XEndEnv
import qualified HasKAL.DetectorUtils.Detector as D
import HasKAL.FrameUtils.FrameUtils
import HasKAL.SearchUtils.Common.CleanDataFinder (cleanDataFinderCore)
import HasKAL.TimeUtils.Function (formatGPS, deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature (GPSTIME, Date, LocalTime)
import HasKAL.WaveUtils.Data (WaveData(..),  mkWaveData, dropWaveData, takeWaveData)
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
    return $ head $ fromMaybe (error "no file in this gps") maybel
  getChannelList file >>= \maybech ->
    return $ Just $ fst . unzip $ fromMaybe (error "no channel in this gps") maybech


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


kagraWaveDataGet :: Int -> Int -> String -> D.Detector -> IO (Maybe WaveData)
kagraWaveDataGet gpsstrt duration chname detector = runMaybeT $ MaybeT $ do
  flist <- kagraDataFind (fromIntegral gpsstrt) (fromIntegral duration) chname
  case flist of
    Nothing -> return Nothing
    Just x -> do
      let headfile = head x
          lastfile = last x {-- stopGPSの判定に必要なので追加 --}
      getSamplingFrequency headfile chname >>= \maybefs ->
        case maybefs of
          Nothing -> return Nothing
          Just fs -> do
            {--  stopGPS も取得するように変更  --}
            getGPSTime headfile >>= \maybegps -> getGPSTime lastfile >>= \maybegps' ->
              case (maybegps, maybegps') of
                (Nothing, _) -> return Nothing
                (_, Nothing) -> return Nothing
                (Just (gpstimeSec, gpstimeNano, dt), Just (gpstimeSec', gpstimeNano', dt')) -> do
            -- getGPSTime headfile >>= \maybegps ->
            --   case maybegps of
            --     Nothing -> return Nothing
            --     Just (gpstimeSec, gpstimeNano, dt) -> do
                  {-- DGSのFWで(gpstimeNano == 0)が保障されているなら"-gpstimeNano*1e-9"は消して良い --}
                  {-- データ先頭が空白、即ち(指定時刻) - (ファイル先頭時刻) < 0 の場合
                         startGPSはファイル先頭と一致、WaveData先頭はファイル先頭と一致(headNum==0)
                      データ先頭が存在、即ち(指定時刻) - (ファイル先頭時刻) >= 0 の場合
                         startGPSは指定時刻と一致、WaveData先頭は指定時刻とファイル先頭の差分だけずれる(headNum>0)
                  --}
                  let (headNum, wdStartGPS) = if ((fromIntegral $ gpsstrt - gpstimeSec) - fromIntegral gpstimeNano*1e-9) < 0
                                                then (0, (gpstimeSec, gpstimeNano))
                                                else (floor $ ((fromIntegral $ gpsstrt - gpstimeSec) - fromIntegral gpstimeNano*1e-9) * fs, (gpsstrt,0))
                  -- let headNum = if (fromIntegral gpsstrt - gpstimeSec) <= 0
                  --                 then 0
                  --                 else floor $ fromIntegral (fromIntegral gpsstrt - gpstimeSec) * fs
                  {-- stopGPSTimeの判定に必要なので追加 --}
                  {-- データ末尾が空白、即ち(ファイル末尾時刻) - (指定時刻+duration) < 0 の場合
                         stopGPSはファイル末尾と一致、WaveData末尾はファイル末尾と一致(lastNum=0)
                      データ末尾が存在、即ち(ファイル末尾時刻) - (指定時刻+duration) >= 0 の場合
                         stopGPSは指定時刻と一致、WaveData末尾は指定時刻とファイル末尾の差分だけずれる(lastNum>0)
                      ただし(gpstimeSec', gpstimeNano')は最終ファイルの先頭なのでdt'を足す事で最終ファイルの末尾にする
                  --}
                  let gpsstop = gpsstrt + duration
                      (dtS', dtN') = formatGPS dt'
                  let (lastNum, wdStopGPS) = if ((fromIntegral $ gpstimeSec' - gpsstop) + fromIntegral gpstimeNano'*1e-9 + dt') < 0
                                               then (0, (gpstimeSec'+dtS', gpstimeNano'+dtN'))
                                               else (floor $ ((fromIntegral $ gpstimeSec' - gpsstop) + fromIntegral gpstimeNano'*1e-9 + dt') * fs, (gpsstop,0))
                  {-- データ先頭または途中に空白があり、且つ末尾に空白がない場合に後ろに余分なデータがついてきてしまう --}
                      -- nduration = floor $ fromIntegral duration * fs
                  DT.sequence $ Just $ liftM
                    {-- データに空白があるときGPSが正しくなるように変更 --}
                    (mkWaveData detector chname fs wdStartGPS wdStopGPS
                    -- (mkWaveData detector chname fs (gpsstrt, 0) (gpsstrt+duration, 0)
                      {-- データ先頭or末尾に空白があるときデータ点数が正しくなるように変更 --}
                      {-- 先頭、末尾が空白(headNum==lastNum==0)
                             取得したデータをそのままWaveDataに格納 (データ点は V.length)
                          先頭が空白(headNum==0)、末尾はデータ有り(lastNum>0)
                             取得したデータ長から末尾の余分なデータを取り除く(データ点は V.length v - lastNum)
                          先頭はデータ有り(headNum>0)、末尾は空白(lastNum==0)
                             取得したデータ長から先頭の余分なデータを取り除く(データ点は V.length v - headNum)
                          先頭、末尾共にデータ有り(headNum>0, lastNum>0)
                             取得したデータ長から両端の余分なデータを取り除く(データ点は V.length v - headNum - lastNum)
                      --}
                      . V.force . (\v -> V.slice headNum (V.length v - headNum - lastNum) v) . V.concat)
                      -- . V.force . (V.slice headNum nduration) . V.concat)
                        $ forM x (\y -> do
                            maybex <- readFrameV chname y
                            return $ fromJust maybex)


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
                    {-- timendat は[((startGPS,stopGPS), filename)] であって、[((gpsSec, gpsNano), filename)]ではない --}
                    {-- gpsNano==0が保障されていない場合破綻する --}
                    let ts = (fst $ fst y, 0)
                    -- let ts = fst y
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
kagraWaveDataGet0 gpsstrt duration chname = runMaybeT $ MaybeT $ do
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
                  let gpsstop = fromIntegral gpsstrt + fromIntegral duration
                      nInd = nConsecutive $ (fst . unzip) x
                      cdata = consecutive (readFrameV chname) x nInd
                  case length cdata of
                    0 -> return Nothing
                    {-- cdataは[((startGPS,stopGPS), dat)] であって、[((gpsSec, gpsNano), dat)]ではない --}
                    {-- ファイル先頭のgpsNano==0が保障されていない場合破綻する --}
                    1 -> do let timendat = (\((s,e),d) -> ((s,0),d)) $ head cdata
                    -- 1 -> do let timendat = head cdata
                                ts = fst timendat
                                xvec = snd timendat
                                nlen = V.length xvec
                                te = formatGPS (deformatGPS ts + fromIntegral nlen/fs)
                                element =  mkWaveData D.General chname fs ts te xvec
                                headNum = checkStartGPSW (fromIntegral gpsstrt) element
                                endNum = checkStopGPSW (fromIntegral gpsstop) element
                            return $ Just $ dropWaveData headNum $ takeWaveData (nlen-endNum) element
                    {-- cdataは[((startGPS,stopGPS), dat)] であって、[((gpsSec, gpsNano), dat)]ではない --}
                    {-- ファイル先頭のgpsNano==0が保障されていない場合破綻する --}
                    _ -> do let timendat = zeropadding fs $ map (\((s,e),d) -> ((s,0),d)) cdata
                    -- _ -> do let timendat = zeropadding fs cdata
                                ts = fst timendat
                                xvec = snd timendat
                                nlen = V.length xvec
                                te = formatGPS (deformatGPS ts + fromIntegral nlen/fs)
                                element =  mkWaveData D.General chname fs ts te xvec    --
                                headNum = checkStartGPSW (fromIntegral gpsstrt) element -- ここ3行がまだおかしい
                                endNum = checkStopGPSW (fromIntegral gpsstop) element   --
                            return $ Just $ dropWaveData headNum $ takeWaveData (nlen-endNum) element



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
  {-- stopGPS = startGPS + N * fs なのでV.lengthから1引かなくて良い --}
  let x = [(deformatGPS gps, deformatGPS gps + fromIntegral (V.length v)/fs)|(gps, v)<-gpsnv]
  -- let x = [(deformatGPS gps, deformatGPS gps + fromIntegral (V.length v-1)/fs)|(gps, v)<-gpsnv]
      nx= length x
      n0pad = flip map [1, 2..(nx-1)] $ \i-> floor $ (fst (x!!i) - snd (x!!(i-1)))*fs
      v0pad = flip map n0pad $ \x-> V.fromList (replicate x 0.0)
      {-- ファイルのオーバーラップをケア --}
      {-- n0pad<0 の時 n0pad がオーバーラップ点数を表しているのでその分V.dropする --}
      gpsnv' = (snd $ gpsnv!!0) : (flip map [1, 2..(nx-1)] $ \i -> V.drop (negate (n0pad!!(i-1))) (snd $ gpsnv!!i))
      zeroPaddedV = V.concat $ interleave gpsnv' v0pad
      -- zeroPaddedV = V.concat $ interleave (snd (unzip gpsnv)) v0pad
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
   in (((fst . head) tlist, (snd . last) tlist), v) : consecutive f (drop j x) js


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
        {-- indが不連続点のリストなら、最後の不連続点からxの最後の要素までも連続になる --}
        False-> head ind : zipWith (-) (tail ind) (init ind) ++ [length x - last ind]
        -- False-> head ind : zipWith (-) (tail ind) (init ind)


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
      | u <- query XEndEnv.xendenv
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query flist
      wheres $ not' ((ch ! XEndEnv.gpsStart' .<=. value (Just gpsstrt)
        `and'` ch ! XEndEnv.gpsStop'  .<=. value (Just gpsstrt))
        `or'` (ch ! XEndEnv.gpsStart' .>=. value (Just gpsend)
        `and'` ch ! XEndEnv.gpsStop'  .>=. value (Just gpsend)))
      return $ ch ! XEndEnv.fname'


kagraDataFindCore' :: Int32 -> Int32 -> IO [(Maybe Int32, Maybe Int32, Maybe String)]
kagraDataFindCore' gpsstrt duration =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    gpsend = gpsstrt + duration
    flist = relation
      [ u
      | u <- query XEndEnv.xendenv
      ]
    core :: Relation () (Maybe Int32, Maybe Int32, Maybe String)
    core = relation $ do
      ch <- query flist
      wheres $ not' ((ch ! XEndEnv.gpsStart' .<=. value (Just gpsstrt)
        `and'` ch ! XEndEnv.gpsStop'  .<=. value (Just gpsstrt))
        `or'` (ch ! XEndEnv.gpsStart' .>=. value (Just gpsend)
        `and'` ch ! XEndEnv.gpsStop'  .>=. value (Just gpsend)))
      return $ (,,) |$| ch ! XEndEnv.gpsStart' |*| ch ! XEndEnv.gpsStop' |*| ch ! XEndEnv.fname'


kagraDataPointCore :: Int32 -> IO [Maybe String]
kagraDataPointCore gpstime =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    channel = relation
      [ u
      | u <- query XEndEnv.xendenv
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ ch ! XEndEnv.gpsStart' .<=. value (Just gpstime)
      wheres $ ch ! XEndEnv.gpsStop'  .>=. value (Just gpstime)
      return $ ch ! XEndEnv.fname'


kagraDataGPSCore :: Int32 -> IO [Maybe String]
kagraDataGPSCore gpstime =
  handleSqlError' $ withConnectionIO connect $ \conn ->
--  setSqlMode conn
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    channel = relation
      [ u
      | u <- query XEndEnv.xendenv
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ ch ! XEndEnv.gpsStart' .<=. value (Just gpstime)
      wheres $ ch ! XEndEnv.gpsStop'  .>=. value (Just gpstime)
      return $ ch ! XEndEnv.fname'



db2framecache :: Relation () XEndEnv.Xendenv -> IO (Maybe [String])
db2framecache dbname = do
  maybefrlist <- db2framelist dbname
  case catMaybes maybefrlist of
    [] -> return Nothing
    x  -> return (Just x)


db2framelist :: Relation () XEndEnv.Xendenv -> IO [Maybe String]
db2framelist dbname =
  handleSqlError' $ withConnectionIO connect $ \ conn ->
  runQuery' conn (relationalQuery core) ()
    where
      core = relation $ do
        lists <- query dbname
        return $ lists ! XEndEnv.fname'


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




