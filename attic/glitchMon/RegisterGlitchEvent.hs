{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}


module RegisterGlitchEvent
( registGlitchEvent2DB
)
where

import Database.Relational.Query (relation, value, InsertQuery, derivedInsertQuery, (|$|), (|*|), Pi, typedInsert, Insert)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC (quickQuery', runRaw, fromSql, rollback)
import Database.HDBC.Record (runInsertQuery, runInsert)
import Database.HDBC.Query.TH

import HasKAL.FrameUtils.FrameUtils (getGPSTime, getChannelList, getSamplingFrequency)

import Data.List (isInfixOf, (!!))
--import Data.Int (Int32)

import Control.Monad (forM)
import HasKAL.DataBaseUtils.DataSource (connect)
--import HasKAL.DataBaseUtils.Framedb (Framedb, framedb, tableOfFramedb)
import qualified HasKAL.DataBaseUtils.Framedb as Framedb

import Glitchdb (Glitchdb, glitchdb, tableOfGlitchdb)
import qualified Glitchdb as Glitchdb
import System.Process (rawSystem)
import System.Environment (getArgs)
import PipelineFunction
import Data (TrigParam(..))
import Data.Int (Int32)

registGlitchEvent2DB :: TrigParam -> IO()
registGlitchEvent2DB p = handleSqlError' $ withConnectionIO connect $ \conn -> do
--    setSqlMode conn
    let sqlstate = insertGlitchdb (detector p) (event_gpsstarts p) (event_gpsstartn p) (event_gpsstops p) (event_gpsstopn p) (duration p) (energy p) (central_frequency p) (snr p) (significance p) (latitude p) (longitude p) (chname p) (sampling_rate p) (segment_gpsstarts p) (segment_gpsstartn p) (segment_gpsstops p) (segment_gpsstopn p) (dq_flag p) (pipeline p)
    putStrLn $ "SQL: " ++ show sqlstate
      --runInsertQuery conn sqlstate ()
    rawSystem "mysql" ["-uroot", "-e", show sqlstate]
    rollback conn
--    print num




setSqlMode conn = do
  mode <- quickQuery' conn "SELECT @@SESSION.sql_mode" []
  newmode <- case mode of
      [[sqlval]] ->
          let val = fromSql sqlval in
              if "IGNORE_SPACE" `isInfixOf` val
                  then return val
                  else return $ val ++ ",IGNORE_SPACE"
      _          ->
          error "failed to get 'sql_mode'"
  runRaw conn $ "SET SESSION sql_mode = '" ++ newmode ++ "'"



insertGlitchdb :: Maybe String -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe String -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe String -> InsertQuery ()
insertGlitchdb d t00 t01 t10 t11 dur e fc snr s lat lon ch fs s00 s01 s10 s11 dq p = derivedInsertQuery piGlitchdb1 . relation $
  return $ Glitchdb1 |$| value d
                     |*| value t00
                     |*| value t01
                     |*| value t10
                     |*| value t11
                     |*| value dur
                     |*| value e
                     |*| value fc
                     |*| value snr
                     |*| value s
                     |*| value lat
                     |*| value lon
                     |*| value ch
                     |*| value fs
                     |*| value s00
                     |*| value s01
                     |*| value s10
                     |*| value s11
                     |*| value dq
                     |*| value p


piGlitchdb1 :: Pi Glitchdb Glitchdb1
piGlitchdb1 = Glitchdb1 |$| Glitchdb.detector'
                        |*| Glitchdb.eventGpsstarts'
                        |*| Glitchdb.eventGpsstartn'
                        |*| Glitchdb.eventGpsstops'
                        |*| Glitchdb.eventGpsstopn'
                        |*| Glitchdb.duration'
                        |*| Glitchdb.energy'
                        |*| Glitchdb.centralFrequency'
                        |*| Glitchdb.snr'
                        |*| Glitchdb.significance'
                        |*| Glitchdb.latitude'
                        |*| Glitchdb.longitude'
                        |*| Glitchdb.chname'
                        |*| Glitchdb.samplingRate'
                        |*| Glitchdb.segmentGpsstarts'
                        |*| Glitchdb.segmentGpsstartn'
                        |*| Glitchdb.segmentGpsstops'
                        |*| Glitchdb.segmentGpsstopn'
                        |*| Glitchdb.dqFlag'
                        |*| Glitchdb.pipeline'


data Glitchdb1 = Glitchdb1
  { f1Detector :: Maybe String
  , f1EventGpsstarts :: Maybe Int32
  , f1EventGpsstartn :: Maybe Int32
  , f1EventGpsstops :: Maybe Int32
  , f1EventGpsstopn :: Maybe Int32
  , f1Duration :: Maybe Double
  , f1Energy :: Maybe Double
  , f1CentralFrequency :: Maybe Double
  , f1Snr :: Maybe Double
  , f1Significance :: Maybe Double
  , f1Latitude :: Maybe Double
  , f1Longitude :: Maybe Double
  , f1Chname :: Maybe String
  , f1SamplingRate :: Maybe Int32
  , f1SegmentGpsstarts :: Maybe Int32
  , f1SegmentGpsstartn :: Maybe Int32
  , f1SegmentGpsstops :: Maybe Int32
  , f1SegmentGpsstopn :: Maybe Int32
  , f1DqFlag :: Maybe Int32
  , f1Pipeline :: Maybe String
  }
$(makeRecordPersistableDefault ''Glitchdb1)




