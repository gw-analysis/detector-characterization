{- code to monitor environmental sensors at KAGRA Xend -}


import HasKAL.WebUtils.FileWatcher (watchNewfile)

main :: IO()
main = do
  let chList = [("K1:PEM-EX_MIC_FLOOR", 2048), ("K1:PEM-EX_MAG_Z_FLOOR", 2048), ("K1:PEM-EX_MAG_Y_FLOOR", 2048), ("K1:PEM-EX_MAG_X_FLOOR", 2048), ("K1:PEM-EX_ACC_NO2_Z_FLOOR", 2048), ("K1:PEM-EX_ACC_NO2_Y_FLOOR", 2048), ("K1:PEM-EX_ACC_NO2_X_FLOOR", 2048), ("K1:PEM-EX_REF", 2048)]
      webhomedir = "/home/rabbithouse/chino/public_html"
      watchdir   = "/data/kagra/xend/R0209"
  watchNewfile "./envDisplay" watchdir watchdir

