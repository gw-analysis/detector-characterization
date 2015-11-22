{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}

module GlitchMon.RegisterGlitchEvent
( registGlitchEvent2DB
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

import GlitchMon.Data (TrigParam(..))
import GlitchMon.PipelineFunction
import qualified GlitchMon.Table as Glitchdb
import GlitchMon.Table (Glitchtbl(..), insertGlitchtble)


registGlitchEvent2DB :: TrigParam -> IO()
registGlitchEvent2DB p = handleSqlError' $ withConnectionIO connect $ \conn -> do
  runInsert conn insertGlitchtbl $ Glitchtbl 0 (detector p) (event_gpsstarts p) (event_gpsstartn p) (event_gpsstops p) (event_gpsstopn p) (duration p) (energy p) (central_frequency p) (snr p) (significance p) (latitude p) (longitude p) (chname p) (sampling_rate p) (segment_gpsstarts p) (segment_gpsstartn p) (segment_gpsstops p) (segment_gpsstopn p) (dq_flag p) (pipeline p)
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


