import System.IO
import WaveletPacket
import MotherWavelet

main = do
  let ndata = 1024
      wp_order = 4
      motherw = Daubechies :: MOTHERWAVELET
      wp_coeff = read_wp_coeff motherw wp_order
  tsdata <- read_ts_data
  wptree <-apply_wp wp_order wp_coeff tsdata
  print $ wptree !! 1
  return wptree


read_ts_data :: IO[Double]
read_ts_data = do
  let tsfilepath = "./tdomain.dat"
  tshandle <- openFile tsfilepath ReadMode
  tsfile <- hGetContents tshandle
  let tsdata = lines tsfile 
  return $ map (read :: String->Double) tsdata

