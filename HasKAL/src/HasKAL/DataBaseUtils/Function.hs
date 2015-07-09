{-# LANGUAGE MonadComprehensions, ScopedTypeVariables #-}

module HasKAL.DataBaseUtils.Function
( kagraDataFind
, kagraDataGet
, kagraDataPoint
, kagraDataFindCore
, kagraDataPointCore
)
where

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

import HasKAL.DataBaseUtils.DataSource                 (connect)
import HasKAL.DataBaseUtils.Framedb                    (framedb)
import qualified HasKAL.DataBaseUtils.Framedb as Frame
import HasKAL.FrameUtils.FrameUtils

import qualified Data.Packed.Vector as DPV
import Control.Monad

kagraDataFind :: Int32 -> Int32 -> String -> IO [String]
kagraDataFind gpsstrt duration chname = do
  flist <- kagraDataFindCore gpsstrt duration chname
  return  [ u
          | (Just u) <- flist
          ]


kagraDataPoint :: Int32 -> String -> IO [String]
kagraDataPoint gpstime chname = do
  flist <- kagraDataPointCore gpstime chname
  return  [ x
          | (Just x) <- flist
          ]


kagraDataGet :: Int -> Int -> String -> IO DPV.Vector Double
kagraDataGet gpsstrt duration chname = do
  flist <- kagraDataFind (fromIntegral gpsstrt) (fromIntegral duration) chname
  let headfile = head flist
  fs <- getSamplingFrequency headfile chname
  let (gpstimeSec, gpstimeNano, dt) = getGPSTime headfile
      headNum = if (fromIntegral gpsstrt - gpstimeSec) <= 0
        then 0
        else floor $ fromIntegral (fromIntegral gpsstrt - gpsTimeSec) * fs
      nduration = floor $ fromIntegral duration * fs
  liftM (DPV.fromList $ take nduration.drop headNum.concat) $ mapM (readFrame channel) fileNames


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


