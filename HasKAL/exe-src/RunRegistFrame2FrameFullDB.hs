


--module RegistDB
--where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (deepseq)
import Control.Monad.State (liftIO)
import Data.Conduit (Sink, Source, await, yield, ($$))
import Data.Text (pack)
import Filesystem.Path (extension)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import HasKAL.DataBaseUtils.FrameFull.DataBaseAdmin (updateFrameDB')
import System.Environment (getArgs)
import System.FSNotify (withManager, watchTree, Event(..), eventPath)
import System.IO (hFlush, stdout)


--runRegistDB watchdir = source watchdir $$ sink

main = do
  dirname <- getArgs >>= \args -> case (length args) of
   1 -> return (head args)
   _ -> error "Usage : RunRegistFrame2FrameFullDB dir"
  source dirname $$ sink

source :: FilePath
       -> Source IO FilePath
source watchdir = do
  x <- liftIO $ withManager $ \manager -> do
    fname <- liftIO newEmptyMVar
    _ <- watchTree manager watchdir (const True)
      $ \event -> case event of
        Removed _ _ -> putStrLn "file removed" >> hFlush stdout
        _           -> case extension (decodeString $ eventPath event) of
                         Just ext -> if (ext==filepart) 
                           then 
                             putStrLn "file downloading" >> hFlush stdout
                           else if (ext==gwf)
                             then do
                               let gwfname = eventPath event
                               putMVar fname gwfname
                             else
                               putStrLn "file extension should be .filepart or .gwf" >> hFlush stdout
    takeMVar fname
  yield x >> source watchdir
  where filepart = pack "filepart"
        gwf = pack "gwf"
  

sink :: Sink String IO ()
sink = do
  c <- await
  case c of 
    Nothing -> do liftIO $ putStrLn "Nothing" >> hFlush stdout
                  sink
    Just fname -> do liftIO $ putStrLn fname >> hFlush stdout
                     x <- liftIO $ updateFrameDB' fname 
                     liftIO $ x `deepseq` return () 
                     sink






