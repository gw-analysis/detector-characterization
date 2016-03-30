





import PlotUtils
import System.Environment (getArgs)
import System.FilePath.Posix (takeBaseName)

main = do
 (year,month,day,ch1file,ch2file) <- getArgs >>= \args -> case (length args) of
  5 -> return (head args, args!!1, args!!2, args!!3, args!!4)
  _ -> error "Usage: correlation_mcgw yyyy mm dd ch1.dat ch2.dat"
 ch1 <- readFile ch1file >>= \x -> return [read b :: Double | [a,b]<-map words (lines  x)]
 ch2 <- readFile ch2file >>= \y -> return [read b :: Double | [a,b]<-map words (lines  y)]
 let ch1base = takeBaseName ch1file
     ch2base = takeBaseName ch2file

 {- scatter plot -}
 let str_title = "GW MC correlation"
     str_legend = "GW vs MC"
     plotfname = "Correlation-GWvsMC_"++ch1base++"-"++ch2base++"_"++year++"-"++month++"-"++day++".png"

 scatterplot'png str_title str_legend plotfname $ zip ch1 ch2



