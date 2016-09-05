
{-# LANGUAGE OverloadedStrings #-}

module HasKAL.IOUtils.Function
    ( stdin2vec
    ) where


import Control.Monad.Trans.Resource (ResourceT,runResourceT)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.ByteString
import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.Combinators as CC
import Data.Conduit.Text as CT
import qualified Data.Text as DT
import Data.Text.ICU.Convert
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Text.Read as TR
import qualified Data.Vector.Storable as VS
import System.IO as SI


stdin2vec :: IO (VS.Vector Double)
stdin2vec = runResourceT $
              CB.sourceHandle SI.stdin
                $= decodeByICU enc
                $= CT.lines
                $= awaitDouble
                $$ CC.sinkVector
  where enc = "ASCII"


decodeByICU :: MonadIO m => String -> Conduit ByteString m DT.Text
decodeByICU = convertByICU toUnicode


encodeByICU :: MonadIO m => String -> Conduit DT.Text m ByteString
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
    Nothing -> awaitDouble
  where   
 --      go t = case readMaybe (DT.pack . strip . DT.unpack $ t) of
    go t = case readMaybe t of
      Just i -> yield i
      Nothing -> awaitDouble
    readMaybe t = case (TR.signed TR.rational) t of
      (Right (i, "")) -> Just i
      (Right (_, _)) -> Nothing
      (Left _) -> Nothing


lstrip [] = []
lstrip xs'@(x:xs) | x == ' '  = lstrip xs
                  | otherwise = xs'


rstrip = Prelude.reverse . lstrip . Prelude.reverse


strip = lstrip . rstrip



