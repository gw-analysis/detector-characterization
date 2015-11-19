{-# LANGUAGE MonadComprehensions, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module HasKAL.DataBaseUtils.FrameFull.Function
( db2framelist
, db2framecache
, kagraChannelList
, kagraDataFind
, kagraDataFind'
, kagraDataGet
, kagraDataGet'
, kagraDataGPS
, kagraDataPoint
, kagraDataFindCore
, kagraDataPointCore
, kagraTimeDataGet
)
where


import Control.Applicative ((<$>), Applicative(pure, (<*>)))
import Control.Monad
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Data.Int                   (Int32)
import Data.List                  (isInfixOf)
import Data.Maybe                 (fromJust, fromMaybe, catMaybes)
import qualified Data.Packed.Vector as DPV
import qualified Data.Traversable as DT
import qualified Data.Vector.Storable as V
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
import Database.Relational.Query.Pure (ProductConstructor,  productConstructor)
import Database.Record
import Database.Record.ToSql
import Foreign.C.Types
import HasKAL.DataBaseUtils.KAGRADataSource (connect)
import HasKAL.DataBaseUtils.FrameFull.Table (Framefull(..), insertFramefull)
import qualified HasKAL.DataBaseUtils.FrameFull.Table as FrameFull
import HasKAL.FrameUtils.FrameUtils
import System.IO.Unsafe (unsafePerformIO)


kagraChannelList :: Int32 -> IO (Maybe [String])
kagraChannelList gpstime = runMaybeT $ MaybeT $ do
  file <- kagraDataGPS gpstime >>= \maybel ->
    case maybel of 
      Nothing -> print "no file in this GPS time." >> return Nothing
      Just xs  -> return $ Just (head xs)
  getChannelList (fromJust file) >>= \maybech ->
     case maybech of
       Nothing -> print "no channel in this GPS time." >> return Nothing
       Just xs -> return $ Just (fst . unzip $ xs)


kagraDataFind :: Int32 -> Int32 -> String -> IO (Maybe [String])
kagraDataFind gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFindCore gpsstrt duration
  let out = [ u
            | (Just u) <- flist, (not . null) $ existChannel chname u
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


kagraDataFind' :: Int32 -> Int32 -> String -> IO (Maybe [((Int, Int), String)])
kagraDataFind' gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFindCore' gpsstrt duration
  let out = [ ((fromIntegral ts :: Int, fromIntegral te :: Int), u)
            | (Just ts, Just te, Just u) <- flist, (not . null) $ existChannel chname u
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


kagraDataPoint :: Int32 -> String -> IO (Maybe [String])
kagraDataPoint gpstime chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataPointCore gpstime
  let out = [ u
            | (Just u) <- flist, (not . null) $ existChannel chname u
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


existChannel chname fname = unsafePerformIO $ do
  getChannelList fname >>= \maybech -> case maybech of 
    Nothing -> return []
    Just x -> let judge = filter (\ch -> ch == chname) $ (fst . unzip) x 
               in case null judge of
                    True -> return []
                    False-> return fname


kagraDataGPS :: Int32 -> IO (Maybe [String])
kagraDataGPS gpstime = runMaybeT $ MaybeT $ do
  flist <- kagraDataGPSCore gpstime
  let out = [ u
            | (Just u) <- flist
            ]
  case out of
    []     -> return Nothing
    x -> return (Just x)


kagraDataGet :: Int -> Int -> String -> IO (Maybe (V.Vector Double))
kagraDataGet gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFind (fromIntegral gpsstrt) (fromIntegral duration) chname
  case flist of
    Nothing -> return Nothing
    Just x -> do
      let headfile = head x
      getSamplingFrequency headfile chname >>= \maybefs ->
        case maybefs of
          Nothing -> return Nothing
          Just fs ->
            getGPSTime headfile >>= \maybegps ->
              case maybegps of
                Nothing -> return Nothing
                Just (gpstimeSec, gpstimeNano, dt) -> do
                  let headNum = if (fromIntegral gpsstrt - gpstimeSec) <= 0
                                  then 0
                                  else floor $ fromIntegral (fromIntegral gpsstrt - gpstimeSec) * fs
                      nduration = floor $ fromIntegral duration * fs
                  DT.sequence $ Just $ liftM (V.force . (V.slice headNum nduration) . V.concat)
                    $ forM x (\y -> do
                        maybex <- readFrameV chname y
                        return $ fromJust maybex)


kagraDataGet' :: Int -> Int -> String -> IO (Maybe (V.Vector CDouble))
kagraDataGet' gpsstrt duration chname = runMaybeT $ MaybeT $ do
  flist <- kagraDataFind (fromIntegral gpsstrt) (fromIntegral duration) chname
  case flist of
    Nothing -> return Nothing
    Just x -> do
      let headfile = head x
      getSamplingFrequency headfile chname >>= \maybefs ->
        case maybefs of
          Nothing -> return Nothing
          Just fs ->
            getGPSTime headfile >>= \maybegps ->
              case maybegps of
                Nothing -> return Nothing
                Just (gpstimeSec, gpstimeNano, dt) -> do
                  let headNum = if (fromIntegral gpsstrt - gpstimeSec) <= 0
                                  then 0
                                  else floor $ fromIntegral (fromIntegral gpsstrt - gpstimeSec) * fs
                      nduration = floor $ fromIntegral duration * fs
                  DT.sequence $ Just $ liftM (V.force . (V.slice headNum nduration) . V.concat)
                    $ forM x (\y -> do
                        maybex <- readFrameVCD chname y
                        return $ fromJust maybex)


kagraTimeDataGet :: Int -> Int -> String -> IO (Maybe [((Int,Int),V.Vector Double)])
kagraTimeDataGet gpsstrt duration chname = runMaybeT $ MaybeT $ do
  tf <- kagraDataFind' (fromIntegral gpsstrt) (fromIntegral duration) chname
  case tf of
    Nothing -> return Nothing
    Just x -> do
      let headfile = snd . head $ x
      getSamplingFrequency headfile chname >>= \maybefs ->
        case maybefs of
          Nothing -> return Nothing
          Just fs ->
            getGPSTime headfile >>= \maybegps ->
              case maybegps of
                Nothing -> return Nothing
                Just (gpstimeSec, gpstimeNano, dt) -> do
                  let headNum = if (fromIntegral gpsstrt - gpstimeSec) <= 0
                                  then 0
                                  else floor $ fromIntegral (fromIntegral gpsstrt - gpstimeSec) * fs
                      nduration = floor $ fromIntegral duration * fs
                      nInd = nConsecutive $ (fst . unzip) x
                  return $ Just $ consecutive (readFrameV chname) x nInd


consecutive :: (String -> IO (Maybe (V.Vector Double)))
            -> [((Int,Int),String)]
            -> [Int]
            -> [((Int,Int),V.Vector Double)]
consecutive f [] _ = []
consecutive f _ [] = []
consecutive f x (j:js) = do
  let y = take j x
      (tlist, flist) = unzip y
      v = V.concat $ for flist (fromJust . unsafePerformIO . f)
   in (((fst . head) tlist, (snd . last) tlist), v) : consecutive f (drop j x) js


findConsecutiveInd :: [(Int,Int)] -> [Int]
findConsecutiveInd x =
  let (tss,tes) = unzip x
      y = zip (zip (tail tss) (init tes)) [1..]
      judge = filter (\(x,y) -> fst x /= snd x) y
   in case null judge of
        False -> snd . unzip $ judge
        True  -> [0]


nConsecutive :: [(Int,Int)] -> [Int]
nConsecutive x =
  let ind = findConsecutiveInd x
   in case (ind==[0]) of
        True -> [length x]
        False-> head ind : zipWith (-) (tail ind) (init ind)


for = flip map


kagraDataFindCore :: Int32 -> Int32 -> IO [Maybe String]
kagraDataFindCore gpsstrt duration =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    gpsend = gpsstrt + duration
    flist = relation
      [ u
      | u <- query FrameFull.framefull
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query flist
      wheres $ not' ((ch ! FrameFull.gpsStart' .<=. value (Just gpsstrt)
        `and'` ch ! FrameFull.gpsStop'  .<=. value (Just gpsstrt))
        `or'` (ch ! FrameFull.gpsStart' .>=. value (Just gpsend)
        `and'` ch ! FrameFull.gpsStop'  .>=. value (Just gpsend)))
      return $ ch ! FrameFull.fname'


kagraDataFindCore' :: Int32 -> Int32 -> IO [(Maybe Int32, Maybe Int32, Maybe String)]
kagraDataFindCore' gpsstrt duration =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    gpsend = gpsstrt + duration
    flist = relation
      [ u
      | u <- query FrameFull.framefull
      ]
    core :: Relation () (Maybe Int32, Maybe Int32, Maybe String)
    core = relation $ do
      ch <- query flist
      wheres $ not' ((ch ! FrameFull.gpsStart' .<=. value (Just gpsstrt)
        `and'` ch ! FrameFull.gpsStop'  .<=. value (Just gpsstrt))
        `or'` (ch ! FrameFull.gpsStart' .>=. value (Just gpsend)
        `and'` ch ! FrameFull.gpsStop'  .>=. value (Just gpsend)))
      return $ (,,) |$| ch ! FrameFull.gpsStart' |*| ch ! FrameFull.gpsStop' |*| ch ! FrameFull.fname'


kagraDataPointCore :: Int32 -> IO [Maybe String]
kagraDataPointCore gpstime =
  handleSqlError' $ withConnectionIO connect $ \conn ->
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    channel = relation
      [ u
      | u <- query FrameFull.framefull
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ ch ! FrameFull.gpsStart' .<=. value (Just gpstime)
      wheres $ ch ! FrameFull.gpsStop'  .>=. value (Just gpstime)
      return $ ch ! FrameFull.fname'


kagraDataGPSCore :: Int32 -> IO [Maybe String]
kagraDataGPSCore gpstime =
  handleSqlError' $ withConnectionIO connect $ \conn ->
--  setSqlMode conn
  outputResults conn core
  where
    outputResults c q = runQuery' c (relationalQuery q) ()
    channel = relation
      [ u
      | u <- query FrameFull.framefull
      ]
    core :: Relation () (Maybe String)
    core = relation $ do
      ch <- query channel
      wheres $ ch ! FrameFull.gpsStart' .<=. value (Just gpstime)
      wheres $ ch ! FrameFull.gpsStop'  .>=. value (Just gpstime)
      return $ ch ! FrameFull.fname'



db2framecache :: Relation () FrameFull.Framefull -> IO (Maybe [String])
db2framecache dbname = do
  maybefrlist <- db2framelist dbname
  case catMaybes maybefrlist of
    [] -> return Nothing
    x  -> return (Just x)


db2framelist :: Relation () FrameFull.Framefull -> IO [Maybe String]
db2framelist dbname =
  handleSqlError' $ withConnectionIO connect $ \ conn ->
  runQuery' conn (relationalQuery core) ()
    where
      core = relation $ do
        lists <- query dbname
        return $ lists ! FrameFull.fname'


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




