{-# LANGUAGE MonadComprehensions, ScopedTypeVariables #-}

module HasKAL.DataBaseUtils.Function
( db2framelist
, db2framecache
, kagraChannelList
, kagraDataFind
, kagraDataGet
, kagraDataGet'
, kagraDataGPS
, kagraDataPoint
, kagraDataFindCore
, kagraDataPointCore
)
where

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
                                 )
import Database.HDBC.Session     (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC              (quickQuery', runRaw, fromSql)

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


kagraDataGet :: Int -> Int -> String -> IO (Maybe (DPV.Vector Double))
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
--      DT.sequence $ Just $ liftM (DPV.fromList.take nduration.drop headNum.concat) $ mapM (readFrame chname) x
                  DT.sequence $ Just $ liftM (DPV.fromList.take nduration.drop headNum.concat)
                    $ forM x (\y -> do
                        maybex <- readFrame chname y
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
      let val = fromSql sqlval in
        if "IGNORE_SPACE" `isInfixOf` val
          then return val
          else return $ val ++ ", IGNORE_SPACE"
    _          ->
      error "failed to get 'sql_mode'"
  runRaw conn $ "SET SESSION sql_mode = '" ++ newmode ++ "'"


