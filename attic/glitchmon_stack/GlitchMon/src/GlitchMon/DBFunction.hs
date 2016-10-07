{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module GlitchMon.DBFunction
( extractTrigInfoTFSNR
, extractTrigInfoTFSNRSize
, extractTrigDQFlag
, extractTrigLocation
, extractTrigSignificance
, extractTrigSNR
, extractTrigCentralFrequency
, extractTrigSize
, extractTrigEnergyCore
, extractTrigDuration
, extractTrigCGPS
, extractTrigFrequencyBand
, extractTrigGPS
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

import HasKAL.DataBaseUtils.KAGRADataSource (connect)
import HasKAL.DataBaseUtils.FrameFull.Function
import HasKAL.TimeUtils.Function
import GlitchMon.Table (Glitchtbl(..), insertGlitchtbl)
import qualified GlitchMon.Table as Glitch



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


extractTrigInfoTFSNRSize :: Int 
                         -> Int 
                         -> Double 
                         -> Double 
                         -> Double 
                         -> Double 
                         -> IO (Maybe [(Double, Double, Double, Int)])
extractTrigInfoTFSNRSize gpsstart gpsstop snrlow snrhigh flow fhigh = runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigInfoTFSNRSizeCore gpsstart' gpsstop' snrlow snrhigh flow fhigh
  let out = [(fromIntegral t :: Double, f::Double, snr::Double, fromIntegral nsize :: Int)
            | (Just t, Just f, Just snr, Just nsize) <- items
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
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
        `and'` db ! Glitch.snr' .>=. value (Just snrlow)
        `and'` db ! Glitch.snr' .<=. value (Just snrhigh)
      return $ (,,) |$| db ! Glitch.eventGpsstarts' |*| db ! Glitch.centralFrequency' |*| db ! Glitch.snr'


extractTrigInfoTFSNRSizeCore :: Int32 
                             -> Int32 
                             -> Double 
                             -> Double 
                             -> Double 
                             -> Double 
                             -> IO [(Maybe Int32, Maybe Double, Maybe Double, Maybe Int32)]
extractTrigInfoTFSNRSizeCore gpsstart gpsstop snrlow snrhigh flow fhigh=
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () ((Maybe Int32, Maybe Double, Maybe Double, Maybe Int32))
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
        `and'` db ! Glitch.snr' .>=. value (Just snrlow)
        `and'` db ! Glitch.snr' .<=. value (Just snrhigh)
        `and'` db ! Glitch.centralFrequency' .>=. value (Just flow)
        `and'` db ! Glitch.centralFrequency' .<=. value (Just fhigh)
      return $ (,,,) |$| db ! Glitch.eventGpsstarts' |*| db ! Glitch.centralFrequency' |*| db ! Glitch.snr' |*| db ! Glitch.islandSize'



extractTrigGPS :: Int -> Int -> IO (Maybe [(Double,Double)])
extractTrigGPS gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigGPSCore gpsstart' gpsstop'
  let out = [(deformatGPS (fromIntegral bs,fromIntegral bn),deformatGPS (fromIntegral es,fromIntegral en))
            | (Just bs, Just bn, Just es, Just en) <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigGPSCore :: Int32 -> Int32 -> IO [(Maybe Int32, Maybe Int32, Maybe Int32, Maybe Int32)]
extractTrigGPSCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Int32, Maybe Int32, Maybe Int32, Maybe Int32)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ (,,,) |$| db ! Glitch.eventGpsstarts' |*| db ! Glitch.eventGpsstartn' |*| db ! Glitch.eventGpsstops' |*| db ! Glitch.eventGpsstopn'


extractTrigFrequencyBand :: Int -> Int -> IO (Maybe [(Double,Double)])
extractTrigFrequencyBand gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigFrequencyBandCore gpsstart' gpsstop'
  let out = [(a,b)
            | (Just a, Just b) <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigFrequencyBandCore :: Int32 -> Int32 -> IO [(Maybe Double, Maybe Double)]
extractTrigFrequencyBandCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Double, Maybe Double)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ (,) |$| db ! Glitch.eventFmin' |*| db ! Glitch.eventFmax'


extractTrigCGPS :: Int -> Int -> IO (Maybe [Double])
extractTrigCGPS gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigCGPSCore gpsstart' gpsstop'
  let out = [deformatGPS (fromIntegral a,fromIntegral b)
            | (Just a, Just b) <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigCGPSCore :: Int32 -> Int32 -> IO [(Maybe Int32, Maybe Int32)]
extractTrigCGPSCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Int32, Maybe Int32)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ (,) |$| db ! Glitch.eventCgpss' |*| db ! Glitch.eventCgpsn'


extractTrigDuration :: Int -> Int -> IO (Maybe [Double])
extractTrigDuration gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigDurationCore gpsstart' gpsstop'
  let out = [a
            | Just a <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigDurationCore :: Int32 -> Int32 -> IO [Maybe Double]
extractTrigDurationCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Double)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ db ! Glitch.duration'


extractTrigEnergy :: Int -> Int -> IO (Maybe [Double])
extractTrigEnergy gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigEnergyCore gpsstart' gpsstop'
  let out = [a
            | Just a <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigEnergyCore :: Int32 -> Int32 -> IO [Maybe Double]
extractTrigEnergyCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Double)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ db ! Glitch.energy'


extractTrigSize :: Int -> Int -> IO (Maybe [Double])
extractTrigSize gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigSizeCore gpsstart' gpsstop'
  let out = [fromIntegral a
            | Just a <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigSizeCore :: Int32 -> Int32 -> IO [Maybe Int32]
extractTrigSizeCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Int32)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ db ! Glitch.islandSize'


extractTrigCentralFrequency :: Int -> Int -> IO (Maybe [Double])
extractTrigCentralFrequency gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigCentralFrequencyCore gpsstart' gpsstop'
  let out = [a
            | Just a <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigCentralFrequencyCore :: Int32 -> Int32 -> IO [Maybe Double]
extractTrigCentralFrequencyCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Double)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ db ! Glitch.centralFrequency'


extractTrigSNR :: Int -> Int -> IO (Maybe [Double])
extractTrigSNR gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigSNRCore gpsstart' gpsstop'
  let out = [a
            | Just a <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigSNRCore :: Int32 -> Int32 -> IO [Maybe Double]
extractTrigSNRCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Double)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ db ! Glitch.snr'


extractTrigSignificance :: Int -> Int -> IO (Maybe [Double])
extractTrigSignificance gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigSignificanceCore gpsstart' gpsstop'
  let out = [a
            | Just a <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigSignificanceCore :: Int32 -> Int32 -> IO [Maybe Double]
extractTrigSignificanceCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Double)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ db ! Glitch.significance'


extractTrigLocation :: Int -> Int -> IO (Maybe [(Double, Double)])
extractTrigLocation gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigLocationCore gpsstart' gpsstop'
  let out = [(a,b)
            | (Just a, Just b) <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigLocationCore :: Int32 -> Int32 -> IO [(Maybe Double, Maybe Double)]
extractTrigLocationCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Double, Maybe Double)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ (,) |$| db ! Glitch.longitude' |*| db ! Glitch.latitude'


extractTrigDQFlag :: Int -> Int -> IO (Maybe [Double])
extractTrigDQFlag gpsstart gpsstop= runMaybeT $ MaybeT $ do
  let gpsstart' = fromIntegral gpsstart :: Int32
      gpsstop'  = fromIntegral gpsstop :: Int32
  items <- extractTrigDQFlagCore gpsstart' gpsstop'
  let out = [fromIntegral a
            | Just a <- items
              ]
  case out of
    [] -> return Nothing
    x  -> return (Just x)


extractTrigDQFlagCore :: Int32 -> Int32 -> IO [Maybe Int32]
extractTrigDQFlagCore gpsstart gpsstop =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    core :: Relation () (Maybe Int32)
    core = relation $ do
      db <- query Glitch.glitchtbl
      wheres $ db ! Glitch.eventGpsstarts' .>=. value (Just gpsstart)
        `and'` db ! Glitch.eventGpsstarts' .<=. value (Just gpsstop)
      return $ db ! Glitch.dqFlag'



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



--instance ProductConstructor (a -> b -> c -> (a, b, c)) where
--  productConstructor = (,,)
 
--instance (FromSql SqlValue a, FromSql SqlValue b, FromSql SqlValue c)
--         => FromSql SqlValue (a, b, c) where
--  recordFromSql = (,,) <$> recordFromSql <*> recordFromSql <*> recordFromSql
 
--instance (ToSql SqlValue a, ToSql SqlValue b, ToSql SqlValue c)
--         => ToSql SqlValue (a, b, c) where
--  recordToSql = createRecordToSql (\(a, b, c) -> fromRecord a ++ fromRecord b ++ fromRecord c)
-- 



