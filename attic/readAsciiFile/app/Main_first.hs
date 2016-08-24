{-# LANGUAGE OverloadedStrings #-}


import Control.Monad.Trans.Resource (ResourceT,runResourceT)
import Data.Conduit
import Data.Conduit.Combinators as CC
import Data.Conduit.List as CL
import Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as BS
import System.IO as SI
import Data.ByteString

import Data.Text.ICU.Convert
import Control.Monad.Trans
import qualified Data.Text as DT


--import Data.Conduit.Text.Multiplatform
import qualified Data.Text.Read as TR
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Conduit.Text as CT
import Data.Vector.Storable

test0 :: IO ()
test0 = runResourceT $ CB.sourceFile "./mag_z.dat" $= CL.map (BS.append "DATA: ") $$ CB.sinkHandle SI.stdout

convertByICU :: MonadIO m => (Converter -> s -> d) -> String -> Conduit s m d
convertByICU f name = do
        conv <- liftIO $ open name (Just False)
        loop f conv
  where
        loop f conv = await >>= maybe (return ()) (go f conv)
        go f conv s = do
              yield $ f conv s
              loop f conv

encodeByICU :: MonadIO m => String -> Conduit DT.Text m ByteString
encodeByICU = convertByICU fromUnicode

decodeByICU :: MonadIO m => String -> Conduit ByteString m DT.Text
decodeByICU = convertByICU toUnicode


-- 数値入力を受け取ります。
awaitDouble :: Monad m => Conduit DT.Text m Double
awaitDouble = do
     t' <-  await
     case t' of
       Just t -> go t
       Nothing -> awaitDouble
     where   
      go t = case readMaybe (DT.pack . strip . DT.unpack $ t) of
          Just i -> yield i
          Nothing -> awaitDouble
      -- Textを数値に変換します
      readMaybe t = case (TR.signed TR.rational) t of
                     (Right (i, "")) -> Just i
                     (Right (_, _)) -> Nothing
                     (Left _) -> Nothing

-- awaitInt :: Monad m => MaybeT (ConduitM DT.Text DT.Text m) Int
-- awaitInt = MaybeT loop
--   where
--      loop = await >>= maybe (return Nothing) go
--      go t = case readMaybe (DT.pack . strip . DT.unpack $ t) of
--          Just i -> return $ Just i
--          Nothing -> do
--              yield "Not number. Please input numbers again\n"
--              loop
-- 
--      -- Textを数値に変換します
--      readMaybe t = case TR.decimal t of
--                      (Right (i, "")) -> Just i
--                      (Right (_, _)) -> Nothing
--                      (Left _) -> Nothing
-- 

lstrip [] = []
lstrip xs'@(x:xs) | x == ' '  = lstrip xs
                  | otherwise = xs'
rstrip = Prelude.reverse . lstrip . Prelude.reverse
strip = lstrip . rstrip


-- 数値のダブルを受け取ります
--awaitIntDouble :: Monad m => MaybeT (ConduitM DT.Text DT.Text m) (Int, Int)
-- awaitIntDouble = (,) <$>
--   (MaybeT $ yield "Input 1st number\n" *> runMaybeT awaitInt) <*>
--   (MaybeT $ yield "Input 2nd number\n" *> runMaybeT awaitInt)
-- 

--awaitDoubleSingle :: Monad m => MaybeT (ConduitM DT.Text Double m) Double
--awaitDoubleSingle =  MaybeT $ runMaybeT awaitDouble <* yield "\n"
--awaitDoubleSingle = awaitDouble

-- アプリケーション本体です。
-- appConduit :: Monad m => Conduit DT.Text m DT.Text
-- appConduit = loop
--   where
--     loop = runMaybeT awaitDoubleSingle >>= maybe (return ()) go
--     go d = do
--         yield $ toText d 
--         loop
-- 
--     -- 数値をTextに変換します。
--     toText = toStrict . toLazyText . TB.realFloat


main :: IO (Vector Double)
main = do runResourceT $
            CB.sourceHandle SI.stdin
                $= decodeByICU enc
                $= CT.lines
                $= awaitDouble
                $$ CC.sinkVector
  where enc = "ASCII"

-- エントリポイントです。
-- main :: IO ()
-- main = do
--         runResourceT $
--           CB.sourceHandle SI.stdin
--               $= decodeByICU enc
--               $= CT.lines
--               $= appConduit
--               $$ encodeByICU enc
--               =$ CB.sinkHandle SI.stdout
--   where
--     enc = "ASCII"


--main :: IO ()
--main = do
--        runResourceT $
--                CB.sourceHandle SI.stdin $= decodeByICU enc $$ encodeByICU enc =$ CB.sinkHandle SI.stdout
--  where
--        enc = "ASCII"













