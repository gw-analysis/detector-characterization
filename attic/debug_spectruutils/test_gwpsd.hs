import SpectrumUtils
import System.Random

main :: IO()
main = do
	let dat = take 100 $ randomRs (-20,20) $ mkStdGen 1 :: [Double]
	let nfft = 10 :: Int
	let fs = 100.0 :: Double
	let out = gwpsd dat nfft fs
	putStrLn $ show $ take 3 out

