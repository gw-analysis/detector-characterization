{-# LANGUAGE OverloadedStrings #-}


import Control.Monad.Trans.Resource (ResourceT,runResourceT)
import Data.Conduit
import Data.Conduit.Combinators as CC
import Data.Conduit.Binary as CB
import qualified Data.ByteString.Char8 as BS
import System.IO as SI
import Data.ByteString

import Data.Text.ICU.Convert
import Control.Monad.Trans
import qualified Data.Text as DT


import qualified Data.Text.Read as TR
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import Control.Monad.Trans.Maybe
import Data.Conduit.Text as CT
import qualified Data.Vector.Storable as VS

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


lstrip [] = []
lstrip xs'@(x:xs) | x == ' '  = lstrip xs
                  | otherwise = xs'
rstrip = Prelude.reverse . lstrip . Prelude.reverse
strip = lstrip . rstrip



main :: IO (VS.Vector Double)
main = do runResourceT $
            CB.sourceHandle SI.stdin
                $= decodeByICU enc
                $= CT.lines
                $= awaitDouble
                $$ CC.sinkVector
  where enc = "ASCII"









