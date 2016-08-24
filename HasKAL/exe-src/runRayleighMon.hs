import Data.List.Split (splitOn)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.SpectrumUtils
import qualified Numeric.LinearAlgebra as NL
import System.Environment (getArgs)


main = do
  (fs',dtfft',df',idname,fname) <- getArgs >>= \args -> case (length args) of
    5 -> return (head args,args!!1,args!!2,args!!3,args!!4)
    _ -> error "Usage: runRayleighMon fs dtfft df ID onecolumnAsciiFile"

  {-- read data --}
  datstr <- readFile fname >>= \x -> return $ lines x
  let dat = map s2d datstr

      --s2d :: [String] -> Double
      s2d x = read x :: Double

  let h1 = NL.fromList dat -- time series vector
      h0 = h1 -- time series vector for reference spectrum

  {-- parameters --}
  let tmpname = concat $ init $ splitOn "." fname
  let oFile = concat [tmpname,fs',dtfft',df',idname,".png"] -- output file name
      fs    = read fs' :: Double         -- sampling frequenc [Hz]
      dtfft = read dtfft' :: Double            -- data size for FFT [second]
      df    = read df' :: Double            -- frequency resolution [Hz]
      pVals = [0.5, 0.9, 0.95, 0.99] -- quanile
      ch    = idname                 -- channel name for graph's title
      
-- #######################################################
  {-- FFT --}
  let nfft = truncate $ dtfft * fs
      snf = gwOnesidedPSDV h0 nfft fs 
      specgram = gwspectrogramV 0 nfft fs h1
  
  {-- for plot --}
  let clist = [RED, BLUE, PINK, GREEN, CYAN, YELLOW, BLACK]
  let title = "#splitline{RayleighMon: "++ch++"  ("++z++")}{   ("++x++y++")}"
        where x = concat $ zipWith (\c q -> (show c)++"="++(show q)++", ") clist pVals
              y = ""
              z = "dt_{FFT}="++(show dtfft)++"s,  df="++(show df)++"Hz"
  let lineType = concat $ replicate (length pVals) [LinePoint, Line]
      colors = concatMap (replicate 2) clist

  {-- main --}
  let results = rayleighMonV pVals fs nfft (truncate $ df * dtfft) snf specgram
  oPlotV Linear lineType 1 colors ("frequency [Hz]", "normalized noise Lv.") 0.05 
    title oFile ((0,0),(0,10)) $ concatMap (\(x,y) -> [x, y]) results
