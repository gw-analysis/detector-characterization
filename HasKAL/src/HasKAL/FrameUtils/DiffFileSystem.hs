{-*********************************
 *DiffFileSystem.hs
 *Created:2014/07/04
 *Author:Mitsuhiro Asano
 **********************************
Last Modified: 2014/07/04 14:38:16
-}

module HasKAL.FrameUtils.DiffFileSystem
(
diffFileSystem
)
where

import qualified HasKAL.FrameUtils.FileManipulation as HFFM
import Data.List
import System.IO

{-
main = do
--     HFFM.genFileList "/home/asano/work_asano/detchar_asano/diffFileSystem/oldFileList.txt" "/frames/full/"
       hoge <- diffFileSystem "oldFileList.txt" "/frames/full/"
       print hoge       
-}


diffFileSystem :: String -> FilePath -> IO [FilePath]
diffFileSystem oldFileName filePath = do
              handle <- openFile "oldFileList.txt" ReadMode
              contents <- hGetContents handle
              newList <- HFFM.getRecursiveFileSystem filePath
              let oldList = lines contents
           	  oldListLength = length oldList
    	          update = drop oldListLength newList
              return update

