{-# LANGUAGE MonadComprehensions, ScopedTypeVariables #-}

module DBFunction
( extractTrigInfoTSNR
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
                                 , (|*|)
                                 , (|$|)
                                 )
import Database.HDBC.Session     (withConnectionIO, handleSqlError')
import Database.HDBC.Record.Query (runQuery')
import Database.HDBC              (quickQuery', runRaw, fromSql)

--import Data.Int                   (Int32)
import Data.List                  (isInfixOf)
import Data.Maybe                 (fromJust, fromMaybe)
import qualified Data.Packed.Vector as DPV
import qualified Data.Traversable as DT

import HasKAL.DataBaseUtils.DataSource (connect)
import Glitchdb (Glitchdb)
import qualified Glitchdb as Glitch



extractTrigInfoTSNR :: Int -> Int -> Double -> Double -> IO (Maybe [(Double, Double)])
extractTrigInfoTSNR gpsstart gpsstop snrlow snrhigh = runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int
      gpsstop'  = fromIntegral gpsstop :: Int
  items <- extractTrigInfoTSNRCore gpsstart' gpsstop' snrlow snrhigh
  let out = [(fromIntegral u :: Double, v)
            | (Just u, Just v) <- items
            ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigInfoTSNRCore :: Int -> Int -> Double -> Double -> IO [(Maybe Int, Maybe Double)]
extractTrigInfoTSNRCore gpsstart gpsstop snrlow snrhigh =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () ((Maybe Int, Maybe Double))
    core = relation $ do
      db <- query glitchdb
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
        `and'` db ! Glitch.snr' .>=. value (Just snrlow)
        `and'` db ! Glitch.snr' .<=. value (Just snrhigh)
      return $ (,) |$| db ! Glitch.eventGpsstarts' |*| db ! Glitch.snr'





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





