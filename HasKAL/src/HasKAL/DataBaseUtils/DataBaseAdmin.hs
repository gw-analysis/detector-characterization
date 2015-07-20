

{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}


module HasKAL.DataBaseUtils.DataBaseAdmin
( updateFrameDB
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Int (Int32)
import Data.List (isInfixOf,  (!!))
import Data.Maybe (fromJust, fromMaybe)
import Database.HDBC.Session (withConnectionIO', withConnectionIO)
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC (quickQuery', runRaw, fromSql, rollback)
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
import qualified HasKAL.DataBaseUtils.DataSource as DD
import HasKAL.DataBaseUtils.Framedb (Framedb,  framedb,  tableOfFramedb)
import qualified HasKAL.DataBaseUtils.Framedb as Framedb
import HasKAL.FrameUtils.FrameUtils (getGPSTime, getChannelList, getSamplingFrequency)
import System.Directory (doesFileExist)
import qualified System.IO as IO
import System.Process (rawSystem)



updateFrameDB fname = doesFileExist fname >>= \b ->
  if b then withConnectionIO' DD.connect $ \conn -> do
         runResourceT $ source fname $$ sink
         rollback conn
       else error "file not found."


source = myreadFile


sink = do
  c <- await
  case c of
    Nothing -> return ()
    Just fname -> do
      maybegps <- liftIO $ getGPSTime fname
      case maybegps of
        Nothing -> return ()
        Just (gpsstrt', gpsstrtnano', duration') -> do
          let gpsstrt = fromIntegral gpsstrt' :: Int32
              duration = fromIntegral (truncate duration') :: Int32
              gpsend = gpsstrt + duration
          maybechfs <- liftIO $ getChannelList fname
    --      let chfs = fromMaybe ("not valid file.") maybechfs
          case maybechfs of
            Nothing -> return ()
            Just chfs -> do
              liftIO $ forM_ chfs $ \(ch, fs) -> do
                let sqlstate = insertFramedb (Just fname) (Just gpsstrt) (Just gpsend) (Just ch) (Just (truncate fs)) (Just 4)
    --      putStrLn $ "SQL: " ++ show sqlstate
    --      runInsertQuery conn sqlstate ()
                rawSystem "mysql" ["-uroot", "--connect_timeout=5", "-e", show sqlstate]
              sink


myreadFile :: FilePath -> Source (ResourceT IO) String
myreadFile file = bracketP
    (do h <- IO.openFile file IO.ReadMode
        putStrLn $ "{" ++ file ++ " open}"
        return h )
    (\h -> do
        IO.hClose h
        putStrLn $ "{" ++ file ++ " closed}" )
    fromHandle
  where
    fromHandle h = forever $ liftIO (IO.hGetLine h) >>= yield


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


