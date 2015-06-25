

{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}


module HasKAL.DataBaseUtils.DataBaseAdmin
( updateFrameDB
) where


import Database.Relational.Query (relation, value, InsertQuery, derivedInsertQuery, (|$|), (|*|), Pi, typedInsert, Insert)
import Database.HDBC.Session (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC (quickQuery', runRaw, fromSql, rollback)
import Database.HDBC.Record (runInsertQuery, runInsert)
import Database.HDBC.Query.TH

import HasKAL.FrameUtils.FrameUtils (getGPSTime, getChannelList, getSamplingFrequency)
import HasKAL.Misc.StrictMapping (forM')
import Data.List (isInfixOf, (!!))
import Data.Int (Int32)

import Control.Monad (forM_)
import DataSource (connect)
import Framedb (Framedb, framedb, tableOfFramedb)
import qualified Framedb

import System.Process (rawSystem)
import System.Environment (getArgs)


updateFrameDB fname = do
  (gpsstrt', gpsstrtnano', duration') <- getGPSTime fname
  let gpsstrt = fromIntegral gpsstrt' :: Int32
      duration = fromIntegral (truncate duration') :: Int32
      gpsend = gpsstrt + duration
  chfs <- getChannelList fname
--  getChannelList fname
  needless <- forM_ chfs $ \(ch,  fs) -> handleSqlError' $ withConnectionIO connect $ \conn -> do
    setSqlMode conn
--    forM_ chfs $ \(ch, fs) -> do
    let sqlstate = insertFramedb (Just fname) (Just gpsstrt) (Just gpsend) (Just ch) (Just (truncate fs)) (Just 4)
--      putStrLn $ "SQL: " ++ show sqlstate
      --runInsertQuery conn sqlstate ()
    rawSystem "mysql" ["-uroot", "-e", show sqlstate]
    rollback conn
  return ()
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



insertFramedb :: Maybe String -> Maybe Int32 -> Maybe Int32 -> Maybe String -> Maybe Int32 -> Maybe Int32 -> InsertQuery ()
insertFramedb f t0 t1 ch fs dqid = derivedInsertQuery piFramedb1 . relation $
  return $ Framedb1 |$| value f
                    |*| value t0
                    |*| value t1
                    |*| value ch
                    |*| value fs
                    |*| value dqid


piFramedb1 :: Pi Framedb Framedb1
piFramedb1 = Framedb1 |$| Framedb.fname'
                      |*| Framedb.gpsStart'
                      |*| Framedb.gpsStop'
                      |*| Framedb.chname'
                      |*| Framedb.samplingRate'
                      |*| Framedb.dqFlag'


data Framedb1 = Framedb1
  { f1Fname :: Maybe String
  , f1GpsStart :: Maybe Int32
  , f1GpsStop :: Maybe Int32
  , f1Chname :: Maybe String
  , f1SamplingRate :: Maybe Int32
  , f1DqFlag :: Maybe Int32
  }
$(makeRecordPersistableDefault ''Framedb1)







--  framedata :: Maybe String -> Maybe Int32 -> Maybe Int32 -> Maybe String -> Maybe Int32 -> Maybe Int32 -> Framedb1
--  framedata f t0 t1 ch fs dqid = Framedb1
--    { f1Fname = f
--    , f1GpsStart = t0
--    , f1GpsStop = t1
--    , f1Chname = ch
--    , f1SamplingRate = fs
--    , f1DqFlag = dqid
--    }

-- insertFramedb2 :: Insert Framedb1
-- insertFramedb2 = typedInsert tableOfFramedb piFramedb1



--  let chfs' = chfs !! 2
--  let ch' = fst chfs'
--      fs' = snd chfs'
--  handleSqlError' $ withConnectionIO connect $ \conn -> do
--    setSqlMode conn
--    let sqlstate = insertFramedb (Just fname) (Just gpsstrt) (Just gpsend) (Just ch') (Just (truncate fs')) (Just 4)
--    rawSystem "mysql" ["-uroot", "-e", show sqlstate]
--    --putStrLn $ "SQL: " ++ show sqlstate
-- --   runInsertQuery conn sqlstate ()
--    rollback conn


--  handleSqlError' $ withConnectionIO connect $ \conn -> do
--    setSqlMode conn
--    runInsert conn insertFramedb2 (framedata (Just fname) (Just gpsstrt) (Just gpsend) (Just ch') (Just (truncate fs')) (Just 4))
--    rollback conn


