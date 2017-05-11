
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module HasKAL.IOUtils.Function
    ( -- output : Vector Storable
      stdin2vec
    , dat2vec
    , stdin2vecs
    , dat2vecs
      -- output : Conduit
    , stdin2chunkV
    , dat2chunkV
       -- input : data file
    , loadASCIIdata
    , loadASCIIdataM
    , loadASCIIdataCV
    , loadASCIIdataRV
       -- helper function
    , splitLines
    , fixLines
    ) where


import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as B
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Text as CT
import qualified Data.Text as DT
import Data.Text.ICU.Convert
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Text.Read as TR
import qualified Data.Vector.Storable as VS
import System.IO as SI
import Numeric.LinearAlgebra
import Numeric.GSL.LinearAlgebra (fromFile, fileDimensions)
import System.IO.Unsafe (unsafePerformIO)


--dat2chunkV :: Int -> FilePath -> Conduit Double IO (VS.Vector Double)
dat2chunkV n fpath = CB.sourceFile fpath
                       $= decodeByICU enc
                       $= CT.lines
                       $= awaitDouble
                       $= CC.conduitVector n
  where enc = "ASCII"


stdin2chunkV :: Int -> Conduit Double IO (VS.Vector Double)
stdin2chunkV n = CB.sourceHandle SI.stdin
                     $= decodeByICU enc
                     $= CT.lines
                     $= awaitDouble
                     $= CC.conduitVector n
  where enc = "ASCII"

stdin2vec :: IO (VS.Vector Double)
stdin2vec = runResourceT $
              CB.sourceHandle SI.stdin
                $= decodeByICU enc
                $= CT.lines
                $= awaitDouble
                $$ CC.sinkVector
  where enc = "ASCII"


dat2vec :: FilePath -> IO (VS.Vector Double)
dat2vec file = runResourceT $
              CB.sourceFile file
                $= decodeByICU enc
                $= CT.lines
                $= awaitDouble
                $$ CC.sinkVector
  where enc = "ASCII"


stdin2vecs :: IO [VS.Vector Double]
stdin2vecs = do lst <- runResourceT $
                  CB.sourceHandle SI.stdin
                  $= decodeByICU enc
                  $= CT.lines
                  $= awaitDoubles
                  $$ CC.sinkList
                let a = matrix (length . head $ lst) $ concat lst
                return $ toColumns a
  where enc = "ASCII"


dat2vecs :: FilePath -> IO [VS.Vector Double]
dat2vecs file = do lst <- runResourceT $
                     CB.sourceFile file
                       $= decodeByICU enc
                       $= CT.lines
                       $= awaitDoubles
                       $$ CC.sinkList
                   let a = matrix (length . head $ lst) $ concat lst
                   return $ toColumns a
  where enc = "ASCII"


decodeByICU :: MonadIO m => String -> Conduit B.ByteString m DT.Text
decodeByICU = convertByICU toUnicode


encodeByICU :: MonadIO m => String -> Conduit DT.Text m B.ByteString
encodeByICU = convertByICU fromUnicode


convertByICU :: MonadIO m => (Converter -> s -> d) -> String -> Conduit s m d
convertByICU f name = do
        conv <- liftIO $ open name (Just False)
        loop f conv
  where
        loop f conv = await >>= maybe (return ()) (go f conv)
        go f conv s = do
              yield $ f conv s
              loop f conv


awaitDouble :: Monad m => Conduit DT.Text m Double
awaitDouble = do
  t' <-  await
  case t' of
    Just t -> go t
    Nothing -> return ()
  where
 --      go t = case readMaybe (DT.pack . strip . DT.unpack $ t) of
    go t = case DT.unpack t of
             "q" -> return ()
             _   -> case readMaybe t of
                      Just i -> yield i >> awaitDouble
                      Nothing -> return ()
    readMaybe t = case (TR.signed TR.rational) t of
      (Right (i, "")) -> Just i
      (Right (_, _)) -> Nothing
      (Left _) -> Nothing


awaitDoubles :: Monad m => Conduit DT.Text m [Double]
awaitDoubles = do
  t' <-  await
  case t' of
    Just t -> go t
    Nothing -> return ()
  where
 --      go t = case readMaybe (DT.pack . strip . DT.unpack $ t) of
    go t = case DT.unpack t of
             "q" -> return ()
             _   -> case readMaybes t of
                      Just i -> yield i >> awaitDoubles
                      Nothing -> return ()
    readMaybes t = do
      let t' = words (DT.unpack t)
          out = flip map t' $ \x ->
                  case (TR.signed TR.rational) (DT.pack x) of
                    (Right (i, "")) -> Just i
                    (Right (_, _))  -> Nothing
                    (Left _)        -> Nothing
      case elem Nothing out of
        True  -> Nothing
        False -> sequence out

loadASCIIdata :: FilePath -> [[Double]]
loadASCIIdata x = unsafePerformIO $ fmap (map (map (\x->read x :: Double).words). splitLines) (readFile x)


loadASCIIdataM :: FilePath -> Matrix Double
loadASCIIdataM x = unsafePerformIO $ do
  rc <- fileDimensions x
  fromFile x rc


loadASCIIdataCV :: FilePath -> [Vector Double]
loadASCIIdataCV x = unsafePerformIO $ do
  rc <- fileDimensions x
  m <- fromFile x rc
  return $ toColumns m


loadASCIIdataRV :: FilePath -> [Vector Double]
loadASCIIdataRV x = unsafePerformIO $ do
  rc <- fileDimensions x
  m <- fromFile x rc
  return $ toRows m


-- file: ch04/SplitLines.hs
splitLines :: String -> [String]


-- file: ch04/SplitLines.hs
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []


isLineTerminator c = c == '\r' || c == '\n'


-- file: ch04/SplitLines.hs
fixLines :: String -> String
fixLines input = unlines (splitLines input)


lstrip [] = []
lstrip xs'@(x:xs) | x == ' '  = lstrip xs
                  | otherwise = xs'


rstrip = Prelude.reverse . lstrip . Prelude.reverse


strip = lstrip . rstrip
