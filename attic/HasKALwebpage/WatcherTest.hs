import Watcher
import Data

main :: IO()
main = do
    let chList = [("K1:PEM-EX_MIC_FLOOR", 2048), ("K1:PEM-EX_MAG_Z_FLOOR", 2048), ("K1:PEM-EX_MAG_Y_FLOOR", 2048), ("K1:PEM-EX_MAG_X_FLOOR", 2048), ("K1:PEM-EX_ACC_NO2_Z_FLOOR", 2048), ("K1:PEM-EX_ACC_NO2_Y_FLOOR", 2048), ("K1:PEM-EX_ACC_NO2_X_FLOOR", 2048), ("K1:PEM-EX_REF", 2048)]
        webdir = "/Users/kazu/kagra/detector-characterization/attic/HasKALwebpage"
        param = Param { sethome = "./"
                      , setsave = "env_image"
                      , setsaveLatest = "end_image_latest"
                      , channellist = chList
                      , filetype = ".png"
                      }
    watch param webdir

