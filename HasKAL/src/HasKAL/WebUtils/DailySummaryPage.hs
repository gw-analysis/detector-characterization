



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



import Data.List (foldl', foldl1', intercalate, elemIndex, isInfixOf, nub, delete)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, createDirectoryIfMissing, createDirectory)
import System.FilePath ((</>))
import System.Environment (getEnv)

type ChName = String
type MonName = String


filterRule :: [(MonName, ChName)] -> [(MonName, ChName)]
filterRule = myfilter.nub
  where 
    -- enabled filters
    myfilter = rule1 .
               rule2 .
               rule3 .
               rule4 .
               rule5 .
               rule6 .
               rule7 .
               rule8 . 
               rule9 .
               rule10 .
               rule11

    -- each filters
    rule1 = limitMonitors "K1:GRD-PSL_STATE_N" ["TimeSeries"]
    rule2 = limitMonitors "K1:GRD-IMC_LOCK_STATE_N" ["TimeSeries"]
    rule3 = limitMonitors "K1:GRD-IFO_STATE_N" ["TimeSeries"]
    rule4 = limitMonitors "K1:GRD-LSC_LOCK_STATE_N" ["TimeSeries"]
    rule5 = limitMonitors "K1:GRD-VIS_BS_STATE_N" ["TimeSeries"]
    rule6 = limitMonitors "K1:GRD-VIS_MCE_STATE_N" ["TimeSeries"]
    rule7 = limitMonitors "K1:GRD-VIS_MCI_STATE_N" ["TimeSeries"]
    rule8 = limitMonitors "K1:GRD-VIS_MCO_STATE_N" ["TimeSeries"]
    rule9 = limitMonitors "K1:GRD-VIS_PR3_STATE_N" ["TimeSeries"]
    rule10= limitMonitors "K1:GRD-MICH_LOCK_STATE_N" ["TimeSeries"]
    rule11= limitMonitors "K1:LSC-MICH_ERR_CAL_OUT_DQ" ["SensMon","RMSMon","RMon","SRMon","Spectrum","Spectrogram","LT","RangeMonNSNS"]


limitMonitors :: ChName -> [MonName] -> [(MonName, ChName)] -> [(MonName, ChName)]
limitMonitors ch mons orig = multiDelete del orig
  where del = multiDelete (map (flip exacta ch) mons) $ filter ((==ch).snd) orig


exacta :: MonName -> ChName -> (MonName, ChName)
exacta x y = (x, y)


multiDelete :: [(MonName, ChName)] -> [(MonName, ChName)] -> [(MonName, ChName)]
multiDelete del orig = foldl' (\bs a -> delete a bs) orig del


genDailySummaryPage dir date chs mons subsystem ncol = do
--  chs <- readFile chlist >>= \x -> return $ lines x
--  mons <- readFile monlist >>= \x -> return $ lines x
  home <- getEnv "HOME"
--  let pth = init $ splitOn "/" dir
--  _ <- recurrentCreateDirectory ([home,"public_html"]++pth) pth
  createDirectoryIfMissing True (home++"/public_html/"++dir)
  let pageTitle = "HasKAL: Daily Summary Page"
      chmon = filterRule [(x,y)| y<-chs, x<-mons]
      fname = [snd c++"-"++date++"_"++fst c|c<-chmon]
      fnamepng = ["." </> x++".png"|x<-fname]
      fnamehtmledCh = home </> "public_html" </> dir </> date++"_"++subsystem++".html"
      relativepthCh = "." </> date++"_"++subsystem++".html"
      fnamehtmledMo = home </> "public_html" </> dir </> date++"_"++subsystem++"_mon.html"
      relativepthMo = "." </> date++"_"++subsystem++"_mon.html"
      nf = length fname
      tables = zipWith (\x y -> addTelement x y) fnamepng fnamepng
      tagname = [snd c++":"++fst c|c<-chmon]
      titles = map addTableTitle tagname
      contentsCh = startHTML
              ++ addHEAD
              ++ addStyle
              ++ startBODY
--              ++ addTitle pageTitle 
              ++ setDIVa
              (  
                 addDate date
              ++ addBR
--              ++ addBR
--              ++ addLayout relativepthCh relativepthMo
              ++ addLayout relativepthCh relativepthMo
              )
--              ++ addBR
--              ++ addBR
--              ++ addSubs (setCenter subsystem)
              ++ concat (for chs $ \ch -> layoutChannelBase ch titles tables ncol)
              ++ endHTML
      contentsMo = startHTML
              ++ addHEAD
              ++ addStyle
              ++ startBODY
              ++ addTitle pageTitle
              ++ setDIVa
              (   addDate date
              ++ addBR
              ++ addBR
              ++ addLayout relativepthCh relativepthMo
              )
              ++ addBR
              ++ addBR
              ++ addSubs (setCenter subsystem)
              ++ concat (for mons $ \mon -> layoutMonitorBase mon titles tables ncol)
              ++ endHTML
  writeFile fnamehtmledCh contentsCh
  writeFile fnamehtmledMo contentsMo


layoutMonitorBase mon titles tables ncol = do
  let titlesch = [x | x<-titles, isInfixOf (":"++mon) x]
      tablesch= [x | x<-tables, isInfixOf ("_"++mon) x]
   in putName mon
       ++ startTABLE
       ++ startTBODY
       ++ (layoutTitleTable titlesch tablesch ncol)
       ++ endTBODY
       ++ endTABLE
       ++ addBR


layoutChannelBase ch titles tables ncol = do
  let titlesch = [x | x<-titles, isInfixOf ch x]
      tablesch= [x | x<-tables, isInfixOf ch x]
   in putName ch
       ++ startTABLE
       ++ startTBODY
       ++ (layoutTitleTable titlesch tablesch ncol)
       ++ endTBODY
       ++ endTABLE
       ++ addBR

for = flip map


putName = setH3 


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
  ,"div#a{"
  ,"color: #800000;"
  ,"background-color: #ffffff;"
  ,"font-size:150%;"
  ,"font-weight:bold; }"
  ,"div#bw{"
  ,"color: #000000;"
  ,"background-color: #ffffff;"
  ,"font-size:160%;"
  ,"font-weight:bold; }"
  ,"</style>"
  ]

startBODY = "<body>"

addTitle x = concat ["<h1 style=\"color: rgb(51, 51, 255);\">"++x++"</h1>"]

addDate x = concat ["Local Date:<br>"++x]

addSubs  = setDIVbw

addLayout pathch pathmo = concat [
  "Layout:"
  , "<br>"
  , "<a href=\""++pathch++"\"><b>- Channel Order</b></a>"
  , "<br>"
  , "<a href=\""++pathmo++"\"><b>- Monitor Order</b></a>"
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

setH1 x =  concat ["<h1>"++x++"</h1>"]
setH2 x =  concat ["<h2>"++x++"</h2>"]
setH3 x =  concat ["<h3>"++x++"</h3>"]

startDIV = "<div>"
endDIV = "</div>"

setDIVa x = concat ["<div id=\"a\">"++x++"</div>"]
setDIVbw x = concat ["<div id=\"bw\">"++x++"</div>"]

setCenter x = concat ["<center>"++x++"</center>"]



