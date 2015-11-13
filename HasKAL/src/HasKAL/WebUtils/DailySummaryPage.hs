



module HasKAL.WebUtils.DailySummaryPage
( genDailySummaryPage
, startHTML
, addStyle
, startBODY
, addTitle
, addDate
, addLayout
, addSubs
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



import Data.List (foldl1', intercalate, elemIndex, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, createDirectory)
import System.FilePath ((</>))
import System.Environment (getEnv)

genDailySummaryPage dir date chs mons subsystem ncol = do
--  chs <- readFile chlist >>= \x -> return $ lines x
--  mons <- readFile monlist >>= \x -> return $ lines x
  home <- getEnv "HOME"
--  let pth = init $ splitOn "/" dir
--  _ <- recurrentCreateDirectory ([home,"public_html"]++pth) pth
  createDirectoryIfMissing True (home++"/public_html/"++dir)
  let pageTitle = "HasKAL: Daily Summary Page"
      fname = [c++"-"++date++"_"++m|c<-chs,m<-mons]
      fnamepng = ["." </> x++".png"|x<-fname]
      fnamehtmledCh = home </> "public_html" </> dir </> date++"_"++subsystem++".html"
      relativepthCh = "." </> date++"_"++subsystem++".html"
      fnamehtmledMo = home </> "public_html" </> dir </> date++"_"++subsystem++"_mon.html"
      relativepthMo = "." </> date++"_"++subsystem++"_mon.html"
      nf = length fname
      tables = zipWith (\x y -> addTelement x y) fnamepng fnamepng
      tagname = [c++":"++m|c<-chs,m<-mons]
      titles = map addTableTitle tagname
      contentsCh = startHTML
              ++ addHEAD
              ++ addStyle
              ++ startBODY
              ++ addTitle pageTitle 
              ++ addDate date
              ++ addLayout relativepthCh relativepthMo
              ++ addSubs subsystem
              ++ concat (for chs $ \ch -> layoutChannelBase ch titles tables ncol)
              ++ endHTML
      contentsMo = startHTML
              ++ addHEAD
              ++ addStyle
              ++ startBODY
              ++ addTitle pageTitle 
              ++ addDate date
              ++ addLayout relativepthCh relativepthMo
              ++ addSubs subsystem
              ++ concat (for mons $ \mon -> layoutMonitorBase mon titles tables ncol)
              ++ endHTML
  writeFile fnamehtmledCh contentsCh
  writeFile fnamehtmledMo contentsMo


layoutMonitorBase mon titles tables ncol = do
  let titlesch = [x | x<-titles, isInfixOf (":"++mon) x]
      tablesch= [x | x<-tables, isInfixOf ("_"++mon) x]
   in startTABLE
       ++ startTBODY
       ++ (layoutTitleTable titlesch tablesch ncol)
       ++ endTBODY
       ++ endTABLE


layoutChannelBase ch titles tables ncol = do
  let titlesch = [x | x<-titles, isInfixOf ch x]
      tablesch= [x | x<-tables, isInfixOf ch x]
   in startTABLE
       ++ startTBODY
       ++ (layoutTitleTable titlesch tablesch ncol)
       ++ endTBODY
       ++ endTABLE


for = flip map


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

addHEADJ phReloadTime = concat [
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


addHEAD = concat [
  "<head>"
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

addTitle x = concat ["<h1 style=\"color: rgb(51, 51, 255);\">"++x++"</h1>"]

addDate x = concat ["<h2>Local Date :"++x++"</h2>"]

addSubs x = concat ["<h2>"++x++"</h2>"]

addLayout pathch pathmo = concat [
  "<h2>"
  , "Layout"
  , "<br>"
  , "<a href=\""++pathch++"\"><b>- Channel Order</b></a>"
  , "<br>"
  , "<a href=\""++pathmo++"\"><b>- Monitor Order</b></a>"
  , "</h2>"
  ]

addBR = "<br>"

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




