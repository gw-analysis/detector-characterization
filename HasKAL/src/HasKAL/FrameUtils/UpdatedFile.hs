{-*********************************
 *filecheck.hs
 *Created:2014/06/24
 *Author:Mitsuhiro Asano
 **********************************
Last Modified: 2014/07/04 11:08:27
-}

module HasKAL.FrameUtils.UpdatedFile
(
updatedFile
)
where

import qualified HasKAL.FrameUtils.FileManipulation as HFFM
import Data.List
import System.IO

{-
--Main Part
main = do
     hoge <- updatedFile "/frames/full/10857/" "/frames/full/10861/"
     print hoge
-}

updatedFile :: FilePath -> FilePath -> IO [FilePath]
updatedFile filePath1 filePath2 = do
	       oldList <- HFFM.getRecursiveFileSystem filePath1
	       newList <- HFFM.getRecursiveFileSystem filePath2 
	       let fileList = oldList ++ newList
   	           check ::[String] -> [String]
		   check [] = []
		   check (x:xs) | elem x xs = x : (check xs)
				| otherwise = check xs
		   correspond = check fileList
		   update = filter (`notElem` correspond) newList
	       return update
  
