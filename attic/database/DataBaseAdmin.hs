

{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}


module DataBaseAdmin
( updateFrameDBfromcache
, updateFrameDB
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

import KAGRADataSource (connect)
import qualified KAGRADataSource as DD
import XEndEnv (Xendenv(..), insertXendenv)
import qualified XEndEnv as XEndEnv


import HasKAL.FrameUtils.FrameUtils (getGPSTime, getChannelList, getSamplingFrequency)
import System.Directory (doesFileExist)
import qualified System.IO as IO
import System.Process (rawSystem)



updateFrameDB fname = doesFileExist fname >>= \b ->
  if b then withConnectionIO' DD.connect $ \conn -> do
         runResourceT $ source fname $$ sink conn
         rollback conn
       else error "file not found."


updateFrameDBfromcache fname = doesFileExist fname >>= \b ->
  if b then withConnectionIO' DD.connect $ \conn -> do
         runResourceT $ sourcefromcache fname $$ sink conn
         rollback conn
       else error "file not found."


source = yield

sourcefromcache = myreadFile


sink conn = do
  c <- await
  case c of
    Nothing -> sink conn
    Just fname -> do
      maybegps <- liftIO $ getGPSTime fname
      case maybegps of
        Nothing -> sink conn
        Just (gpsstrt', gpsstrtnano', duration') -> do
          let gpsstrt = fromIntegral gpsstrt' :: Int32
              duration = fromIntegral (truncate duration') :: Int32
              gpsend = gpsstrt + duration
          liftIO $ runInsert conn insertXendenv $ Xendenv 0 (Just fname) (Just gpsstrt) (Just gpsend)
          liftIO $ commit conn
          sink conn


myreadFile :: FilePath -> Source (ResourceT IO) String
myreadFile file = bracketP
    (do h <- IO.openFile file IO.ReadMode
        return h )
    (\h -> do
        IO.hClose h )
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


