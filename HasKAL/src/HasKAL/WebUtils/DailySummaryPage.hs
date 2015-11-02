



module HasKAL.WebUtils.DailySummaryPage
( genDailySummaryPage
, startHTML
, addStyle
, startBODY
, addTitle
, startTABLE
, startTBODY
, startTR
, addTelement
, endTR
, endTBODY
, endTABLE
, endBODY
, endHTML
) where



import Data.List (foldl1')
import System.FilePath ((</>))
import System.Environment (getEnv)

genDailySummaryPage dir date chlist monlist subsystem ncol = do
  chs <- readFile chlist >>= \x -> return $ lines x
  mons <- readFile monlist >>= \x -> return $ lines x
  home <- getEnv "HOME"
  let fname = [c++"-"++date++"_"++m|c<-chs,m<-mons]
      fnamepng = ["." </> x++".png"|x<-fname]
      fnamehtml = home </> "public_html" </> dir </> date++"_"++subsystem++".html"
      nf = length fname
      tables = zipWith (\x y -> addTelement x y) fnamepng fnamepng
      contents = startHTML 
              ++ addHEAD "32000"
              ++ addStyle 
              ++ startBODY 
              ++ addTitle date subsystem 
              ++ startTABLE
              ++ startTBODY
              ++ (layoutTable [c++":"++m|c<-chs,m<-mons] ncol)
              ++ (layoutTable tables ncol)
              ++ endTBODY
              ++ endTABLE
              ++ endHTML
  writeFile fnamehtml contents


layoutTable [] _ = []
layoutTable tlist n =
   startTR ++ foldl1' (++) (take n tlist) ++ endTR ++ layoutTable (drop n tlist) n


startHTML = concat [
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" >"
  ,"<html>"
  ]

addHEAD phReloadTime = concat [
  "<head>"
  ,"<SCRIPT LANGUAGE=\"JavaScript\">"
  ,"<!--"
  ,"setTimeout(\"location.reload()\","++phReloadTime++");"
  ,"//-->"
  ,"</SCRIPT>"
  ,"<meta http-equiv=\"content-type\" content=\"text/html; charset=ISO-8859-1\">"
  ,"<title>Daily Summary Page</title>"
  ,"</head>"
  ]

addStyle = concat [
  "<style type=\"text/css\">"
  ,"body{"
  ,"font-family: Tahoma,Geneva,sans-serif;"
  ,"color: black;"
  ,"background-color: white;}"
  ,"h1{"
  ,"color: #4169E1;"
  ,"background-color: rgb(255,255,153);"
  ,"padding: 0.5em; border: 1px solid black; }"
  ,"h2{"
  ,"color: #800000;"
  ,"background-color: #cccccc;"
  ,"padding: 0.25em; border: 1px solid black; }"
  ,"h3{"
  ,"color: #800000;"
  ,"background-color: #cccccc;"
  ,"padding: 0.25em; border: 1px solid black; }"
  ,"</style>"
  ]

startBODY = "<body>"

addTitle phLocalTime phSubSystem = concat [
  "<h1 style=\"color: rgb(51, 51, 255);\">HasKAL: Daily Summary Page</h1>"
  ,"<br><h2>Local Time :"++phLocalTime++"</h2>"
  ,"<br><h2>"++phSubSystem++"</h2>"
  ,"<table cellpadding=\"2\" cellspacing=\"2\" border=\"1\" style=\"text-align: left;width: 100%%;\""
  ]

startTABLE = "<table>"

startTBODY = "<tbody>"

startTR = "<tr>"

addTelement phRefFig phSRCFig = concat [
  "<td style=\"vertical-align: top;\">"
  ,"<a href=\""++phRefFig++"\"><img alt=\"\" src=\""++phSRCFig++"\" style=\"border: 0px solid ; width: 300px;\"></a>"
  ,"</td>"
  ]

endTR = "</tr>"

endTBODY = "</tbody>"

endTABLE = "</table>"

endBODY = "</body>"

endHTML = "</html>"




