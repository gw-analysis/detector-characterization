{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module HasKAL.DataBaseUtils.Function
( db2framelist
, db2framecache
, kagraChannelList
, kagraDataFind
, kagraDataFind'
, kagraDataGet
, kagraDataGet'
, kagraDataGPS
, kagraDataPoint
, kagraDataFindCore
, kagraDataPointCore
, kagraTimeDataGet
)
where


import Control.Applicative ((<$>), Applicative(pure, (<*>)))
import Control.Monad
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
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

import Data.Int                   (Int32)
import Data.List                  (isInfixOf)
import Data.Maybe                 (fromJust, fromMaybe, catMaybes)
import qualified Data.Packed.Vector as DPV
import qualified Data.Traversable as DT

import HasKAL.DataBaseUtils.DataSource                 (connect)
import HasKAL.DataBaseUtils.Framedb                    (framedb)
import qualified HasKAL.DataBaseUtils.Framedb as Frame
import HasKAL.FrameUtils.FrameUtils
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)



kagraChannelList :: Int32 -> IO (Maybe [String])
kagraChannelList gpstime = runMaybeT $ MaybeT $ do
  file <- kagraDataGPS gpstime >>= \maybel ->
    return $ head $ fromMaybe (error "no file in this gps") maybel
  getChannelList file >>= \maybech ->
    return $ Just $ fst . unzip $ fromMaybe (error "no channel in this gps") maybech


kagraDataFind :: Int32 -> Int32 -> String -> IO (Maybe [String])
kagraDataFind gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFindCore gpsstrt duration chname
  let out = [ u
            | (Just u) <- flist
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


kagraDataFind' :: Int32 -> Int32 -> String -> IO (Maybe [((Int, Int), String)])
kagraDataFind' gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFindCore' gpsstrt duration chname
  let out = [ ((fromIntegral ts :: Int, fromIntegral te :: Int), u)
            | (Just ts, Just te, Just u) <- flist
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


kagraDataPoint :: Int32 -> String -> IO (Maybe [String])
kagraDataPoint gpstime chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataPointCore gpstime chname
  let out = [ u
            | (Just u) <- flist
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


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


kagraTimeDataGet :: Int -> Int -> String -> IO (Maybe [((Int,Int),V.Vector Double)])
kagraTimeDataGet gpsstrt duration chname = runMaybeT $ MaybeT $ do
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
                  return $ Just $ consecutive (readFrameV chname) x nInd


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
   in snd . unzip $ filter (\(x,y) -> fst x /= snd x) y


nConsecutive :: [(Int,Int)] -> [Int]
nConsecutive x = 
  let ind = findConsecutiveInd x
   in head ind : zipWith (-) (tail ind) (init ind)


for = flip map


kagraDataFindCore :: Int32 -> Int32 -> String -> IO [Maybe String]
kagraDataFindCore gpsstrt duration chname =
  handleSqlError' $ withConnectionIO connect $ \conn ->
--  setSqlMode conn
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    gpsend = gpsstrt + duration
    channel = relation
      [ u
      | u <- query framedb
      , () <- wheres $ u ! Frame.chname' .=. value (Just chname)
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ not' ((ch ! Frame.gpsStart' .<=. value (Just gpsstrt)
        `and'` ch ! Frame.gpsStop'  .<=. value (Just gpsstrt))
        `or'` (ch ! Frame.gpsStart' .>=. value (Just gpsend)
        `and'` ch ! Frame.gpsStop'  .>=. value (Just gpsend)))
      return $ ch ! Frame.fname'


kagraDataFindCore' :: Int32 -> Int32 -> String -> IO [(Maybe Int32, Maybe Int32, Maybe String)]
kagraDataFindCore' gpsstrt duration chname =
  handleSqlError' $ withConnectionIO connect $ \conn ->
--  setSqlMode conn
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    gpsend = gpsstrt + duration
    channel = relation
      [ u
      | u <- query framedb
      , () <- wheres $ u ! Frame.chname' .=. value (Just chname)
      ]
    core :: Relation () (Maybe Int32, Maybe Int32, Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ not' ((ch ! Frame.gpsStart' .<=. value (Just gpsstrt)
        `and'` ch ! Frame.gpsStop'  .<=. value (Just gpsstrt))
        `or'` (ch ! Frame.gpsStart' .>=. value (Just gpsend)
        `and'` ch ! Frame.gpsStop'  .>=. value (Just gpsend)))
      return $ (,,) |$| ch ! Frame.gpsStart' |*| ch ! Frame.gpsStop' |*| ch ! Frame.fname'


kagraDataPointCore :: Int32 -> String -> IO [Maybe String]
kagraDataPointCore gpstime chname =
  handleSqlError' $ withConnectionIO connect $ \conn ->
--  setSqlMode conn
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    channel = relation
      [ u
      | u <- query framedb
      , () <- wheres $ u ! Frame.chname' .=. value (Just chname)
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ ch ! Frame.gpsStart' .<=. value (Just gpstime)
      wheres $ ch ! Frame.gpsStop'  .>=. value (Just gpstime)
      return $ ch ! Frame.fname'


kagraDataGPSCore :: Int32 -> IO [Maybe String]
kagraDataGPSCore gpstime =
  handleSqlError' $ withConnectionIO connect $ \conn ->
--  setSqlMode conn
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    channel = relation
      [ u
      | u <- query framedb
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ ch ! Frame.gpsStart' .<=. value (Just gpstime)
      wheres $ ch ! Frame.gpsStop'  .>=. value (Just gpstime)
      return $ ch ! Frame.fname'



db2framecache :: Relation () Frame.Framedb -> IO (Maybe [String])
db2framecache dbname = do
  maybefrlist <- db2framelist dbname
  case catMaybes maybefrlist of
    [] -> return Nothing
    x  -> return (Just x)


db2framelist :: Relation () Frame.Framedb -> IO [Maybe String]
db2framelist dbname =
  handleSqlError' $ withConnectionIO connect $ \ conn ->
  runQuery' conn (relationalQuery core) ()
    where
      core = relation $ do
        lists <- query dbname
        return $ lists ! Frame.fname'


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




