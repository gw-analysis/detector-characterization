



module HasKAL.WebUtils.DailySummaryPage
( genDailySummaryPage
, startHTML
, addStyle
, startBODY
, addTitle
, startTABLE
, startTBODY
, startTR
, addTableTitle
, addTelement
, endTR
, endTBODY
, endTABLE
, endBODY
, endHTML
) where



import Data.List (foldl1', intercalate, elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, createDirectory)
import System.FilePath ((</>))
import System.Environment (getEnv)

genDailySummaryPage dir date chlist monlist subsystem ncol = do
  chs <- readFile chlist >>= \x -> return $ lines x
  mons <- readFile monlist >>= \x -> return $ lines x
  home <- getEnv "HOME"
  let pth = init $ splitOn "/" dir
  _ <- recurrentCreateDirectory ([home,"public_html"]++pth) pth
  let fname = [c++"-"++date++"_"++m|c<-chs,m<-mons]
      fnamepng = ["." </> x++".png"|x<-fname]
      fnamehtml = home </> "public_html" </> dir </> date++"_"++subsystem++".html"
      nf = length fname
      tables = zipWith (\x y -> addTelement x y) fnamepng fnamepng
      titles = map addTableTitle [c++":"++m|c<-chs,m<-mons]
      contents = startHTML
              ++ addHEAD "32000"
              ++ addStyle
              ++ startBODY
              ++ addTitle date subsystem
              ++ startTABLE
              ++ startTBODY
              ++ (layoutTitleTable titles tables ncol)
              ++ endTBODY
              ++ endTABLE
              ++ endHTML
  writeFile fnamehtml contents


recurrentCreateDirectory _ [] = return ()
recurrentCreateDirectory f (dir:dirs) = 
  doesDirectoryExist dir >>= \b -> case b of
    True -> recurrentCreateDirectory f dirs
    False -> do 
      createDirectory (intercalate "/" (take (succ $ fromMaybe (error "no such dir") (elemIndex dir f)) f))
      recurrentCreateDirectory f dirs


layoutTable [] _ = []
layoutTable tlist n =
   startTR ++ foldl1' (++) (take n tlist) ++ endTR ++ layoutTable (drop n tlist) n


layoutTitleTable [] _ _ = []
layoutTitleTable _ [] _ = []
layoutTitleTable titlelist tablelist n =
   startTR ++ foldl1' (++) (take n titlelist ++ [endTR] ++ [startTR] ++ take n tablelist) ++ endTR
     ++ layoutTitleTable (drop n titlelist) (drop n tablelist) n


addTableTitle title = concat ["<th style=\"vertical-align: top;\">"++title++"</th>"]


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
  ]

startTABLE = "<table cellpadding=\"2\" cellspacing=\"2\" border=\"1\" style=\"text-align: left;width: 100%%;\">"

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




