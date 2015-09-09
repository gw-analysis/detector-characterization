
import Data.Maybe
import qualified Data.Vector.Storable as D (fromList,  length)
import qualified Data.Packed.Matrix as D (rows,  cols)
import Control.Monad ((>>=))
import qualified HasKAL.FrameUtils.FrameUtils as F
import qualified HasKAL.FrameUtils.Function as F

import Data
import Signature
import SensMon


main = do
  let fname = "H-H1_LOSC_4_V1-855318528-4096.gwf"
  F.getChannelList fname >>=
    \maybech -> do
      let (ch, fs) = last $ fromMaybe (error "no valid ch") maybech
      print ch
      F.readFrameV ch fname >>=
        \maybev -> case maybev of
          Nothing -> error "no valid data"
          Just v -> do let (x, y, z) = runSensMon v fs (10*truncate fs)
                       print $ D.cols z
                       print $ D.rows z
                       print $ D.length x
                       print $ D.length y


