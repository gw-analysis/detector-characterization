{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module HasKAL.MonitorUtils.GlitchMon.RegisterGlitchEventSQLite3
( registGlitchEvent2DBSQLite3
)
where


import Control.Monad (forM)
import Data.Int (Int32)
import Data.List (isInfixOf,  (!!))
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC (quickQuery', runRaw, fromSql, rollback, commit)
import Database.HDBC.Record (runInsertQuery, runInsert)
import Database.HDBC.Query.TH
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.Relational.Query ( relation
                                 , value
                                 , InsertQuery
                                 , derivedInsertQuery
                                 , (|$|)
                                 , (|*|)
                                 , Pi
                                 , typedInsert
                                 , Insert
                                 )

import qualified HasKAL.DataBaseUtils.FrameFull.Table as Framedb
import HasKAL.DataBaseUtils.KAGRADataSource (connect)
import HasKAL.FrameUtils.FrameUtils (getGPSTime, getChannelList, getSamplingFrequency)

import System.Environment (getArgs)
import System.Process (rawSystem)

import HasKAL.Misc.Environment(haskalOpt)
import qualified HasKAL.MonitorUtils.GlitchMon.Data as D
import HasKAL.MonitorUtils.GlitchMon.PipelineFunction
--import qualified HasKAL.MonitorUtils.GlitchMon.Table as Glitchdb
--import HasKAL.MonitorUtils.GlitchMon.Table (Glitchtbl(..), insertGlitchtbl)
import HasKAL.DataBaseUtils.KAGRADataSource (defineTableforSqlite3)
import HasKAL.MonitorUtils.GlitchMon.Env (sqlite3dbfilename)


$(defineTableforSqlite3 sqlite3dbfilename "glitchtbl")

--dbfilename = "/home/kazu/Dropbox/CurrentWork/SummerSchool/aaa.db"

registGlitchEvent2DBSQLite3 :: D.TrigParam -> IO()
registGlitchEvent2DBSQLite3 p = handleSqlError' $ withConnectionIO (connectSqlite3 sqlite3dbfilename) $ \conn -> do
  runInsert conn insertGlitchtbl $
    Glitchtbl
      (Just 0)
      (D.detector p)
      (D.event_gpsstarts p)
      (D.event_gpsstartn p)
      (D.event_gpsstops p)
      (D.event_gpsstopn p)
      (D.event_fmin p)
      (D.event_fmax p)
      (D.event_cgpss p)
      (D.event_cgpsn p)
      (D.duration p)
      (D.energy p)
      (D.island_size p)
      (D.central_frequency p)
      (D.snr p)
      (D.significance p)
      (D.latitude p)
      (D.longitude p)
      (D.channel p)
      (D.sampling_rate p)
      (D.segment_gpsstarts p)
      (D.segment_gpsstartn p)
      (D.segment_gpsstops p)
      (D.segment_gpsstopn p)
      (D.dq_flag p)
      (D.pipeline p)
  commit conn


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
