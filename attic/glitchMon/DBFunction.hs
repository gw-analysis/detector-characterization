{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module GlitchMon.DBFunction
( extractTrigInfoTFSNR
)
where

import Control.Applicative ((<$>),  Applicative(pure,  (<*>)))
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
import Database.Relational.Query.Pure (ProductConstructor, productConstructor)
import Database.Record
import Database.Record.ToSql
import Data.Int                   (Int32)
import Data.List                  (isInfixOf)
import Data.Maybe                 (fromJust, fromMaybe)
import qualified Data.Packed.Vector as DPV
import qualified Data.Traversable as DT

import HasKAL.DataBaseUtils.DataSource (connect)
import GlitchMon.Glitchdb (Glitchdb)
import qualified GlitchMon.Glitchdb as Glitch



extractTrigInfoTFSNR :: Int -> Int -> Double -> Double -> IO (Maybe [(Double, Double, Double)])
extractTrigInfoTFSNR gpsstart gpsstop snrlow snrhigh = runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigInfoTFSNRCore gpsstart' gpsstop' snrlow snrhigh
  let out = [(fromIntegral t :: Double, f, snr)
            | (Just t, Just f, Just snr) <- items
            ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigInfoTFSNRCore :: Int32 -> Int32 -> Double -> Double -> IO [(Maybe Int32, Maybe Double, Maybe Double)]
extractTrigInfoTFSNRCore gpsstart gpsstop snrlow snrhigh =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () ((Maybe Int32, Maybe Double, Maybe Double))
    core = relation $ do
      db <- query Glitch.glitchdb
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
        `and'` db ! Glitch.snr' .>=. value (Just snrlow)
        `and'` db ! Glitch.snr' .<=. value (Just snrhigh)
      return $ (,,) |$| db ! Glitch.eventGpsstarts' |*| db ! Glitch.centralFrequency' |*| db ! Glitch.snr'





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




