


import GlitchMon.DBFunction
import GlitchMon.Data
import GlitchMon.GlitchMon
import GlitchMon.GlitchParam
import GlitchMon.GlitchPlot
import GlitchMon.Glitchdb
import GlitchMon.PipelineFunction
import GlitchMon.RegisterGlitchEvent



main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
     4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
     _ -> error "Usage: DailyGlitchMon yyyy mm dd ch"
  let gps = read (time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST") :: Int
      plotfname = ch++"-"++year++"-"++month++"-"++day++"_DailyGlitchMon.png"
      day = 86400
      snrlow = 2
      snrhigh = 1000
      str_title = "GlitchMon: " ++ ch
      str_legend = "GlitchMon"
  extractTrigInfoTFSNR gps (gps+day) snrlow snrhigh >>= \maybelist ->
    case maybelist of
      Nothing -> return ()
      Just x -> scatterplot'png str_title str_legend plotfname x


