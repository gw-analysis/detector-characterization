

import qualified Data.Vector.Storable as V
import HasKAL.DetectorUtils.Detector
import HasKAL.DetectorUtils.Function
import HasKAL.DetectorUtils.DetectorParam
import HasKAL.IOUtils.Function
import HasKAL.SimulationUtils.Injection.Function

import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: GWDetectorResponseInjection [-t(--withTime)] sigfile det phi theta psi n t0 fs stdin"

  (sigfile, det', phi', theta', psi', n', t0', fs') <- case length varArgs of
    8 -> return (varArgs!!0, varArgs!!1, varArgs!!2, varArgs!!3, varArgs!!4, varArgs!!5, varArgs!!6, varArgs!!7)
    _ -> error "Usage: GWDetectorResponseInjection [-t(--withTime)] sigfile det phi theta psi n t0 fs stdin"


  {-- parameters --}
  let det = read det' :: Detector
      phi = read phi' :: Double
      psi = read psi' :: Double
      theta = read theta' :: Double
      fs = read fs' :: Double   -- seconds
      t0 = read t0' :: Double
      n  = read n' :: Int

  let fpct | det==LIGO_Livingston = fplusfcrossts ligoLivingston phi theta psi
           | det==LIGO_Hanford    = fplusfcrossts ligoHanford phi theta psi
           | det==VIRGO           = fplusfcrossts virgo phi theta psi
           | det==KAGRA           = fplusfcrossts kagra phi theta psi
      pol = fst fpct
      delay = snd fpct
      ndelay = floor (delay * fs) + n

  {-- read data --}
  let hpc' = loadASCIIdataCV sigfile
      hpc = (head hpc', hpc'!!1)
      detresp = genDetectorResponse pol hpc

  let  datV  = unsafePerformIO $ stdin2vec
       lenV = V.length datV
       lenDet = V.length detresp

  let snend = ndelay + lenDet

  let dn = lenV - snend

  let inj = case dn < 0 of
       True -> case ndelay < 0 of
                    True ->  let detresp2 =  V.take (lenDet+ndelay+dn) $ V.drop (-ndelay) detresp
                              in addInjsig 0 detresp2 datV
                    False -> let detresp2 = V.take (lenDet+dn) detresp
                              in addInjsig ndelay detresp2 datV
       False-> case ndelay < 0 of
                 True -> let detresp2 = V.take (lenDet+ndelay) $ V.drop (-ndelay) detresp
                          in addInjsig 0 detresp datV
                 False -> addInjsig ndelay detresp datV

  case optTime varOpt of
    False -> mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList inj)
    True  -> mapM_ (\(x,y) -> hPutStrLn stdout $ (show x)++" "++show y)
               $ zip [t0,t0+1/fs..] (V.toList inj)





data Options = Options
  { optTime :: Bool
  } deriving (Show)


defaultOptions  = Options
  { optTime = False
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['t'] ["withTime"]
      ( NoArg (\ opts -> opts {optTime = True}))
      "Usage: filter [-t(--withTime)] channel fs fc ftype stdin"
  ]
