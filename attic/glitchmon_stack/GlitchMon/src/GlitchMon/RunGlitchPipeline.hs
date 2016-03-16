import HasKAL.WebUtils.FileWatcher (watchNewfile)

main :: IO()
main = do
  let resultDir = "/home/rabbithouse/chino/public_html"
      watchDir   = "/data/kagra/xend/R0205"
  watchNewfile "./runGlitchMon" "resultDir" "watchDir"



