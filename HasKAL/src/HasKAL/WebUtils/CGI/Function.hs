

module HasKAL.WebUtils.CGI.Function (
  module Exports
-- Constants
, chlistDir
, pngDir
-- CGI
, uploadFile
, getInputParams
-- HTML Frame
, inputFrame
, resultFrame
, resultFrame'
, dateForm
-- input Form
, fileUpForm
, channelForm
, paramForm
, monitorForm
-- Result body
, genePngTable
, geneChMap
, geneRankTable
) where

import Debug.Trace (trace)
import Network.CGI
import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.List (isSuffixOf, isPrefixOf)
import System.Directory (getDirectoryContents)
import Numeric (showFFloat)

import HasKAL.TimeUtils.GPSfunction (time2gps, gps2localTimetuple, gps2localTime)
import HasKAL.WebUtils.CGI.Data as Exports (Message, ParamCGI(..), MultiSelect(..), MonitorType(..), updateGps, updateMsg)

{--  Constants  --}
chlistDir :: String
chlistDir = "../ch_list/"

pngDir :: String
pngDir = "../mon_images/"

defChList = ["K1:PEM-EX_ACC_NO2_X_FLOOR"
            ,"K1:PEM-EX_ACC_NO2_Y_FLOOR"
            ,"K1:PEM-EX_ACC_NO2_Z_FLOOR"
            ,"K1:PEM-EX_MAG_X_FLOOR"
            ,"K1:PEM-EX_MAG_Y_FLOOR"
            ,"K1:PEM-EX_MAG_Z_FLOOR"
            ,"K1:PEM-EX_MIC_FLOOR"
            ]

{-- CGI --}
uploadFile :: CGI ()
uploadFile = do
  ufile <- getInputFilename "uploadfile"
  content <- getInput "uploadfile"
  liftIO $ case ufile of
    Just "" -> return ()
    Just x -> writeFile (chlistDir++x) (fromJust content)
    Nothing -> return ()

getInputParams :: (MonadIO m, MonadCGI m) => m ParamCGI
getInputParams = do
  script <- scriptName
  files <- liftIO $ getDirectoryContents chlistDir
  lstfile <- getInputDefVar "Default" "lstfile"
  chlst <- liftIO $ case lstfile of
            "Default" -> return defChList
            otherwise -> liftM lines $ readFile (chlistDir++lstfile)
  (gps, locale) <- getInputGPS
  channel1 <- getMultiInput "channel1"
  channel2 <- getMultiInput "channel2"  
  monitors <- getMultiInput "monitor"
  duration <- getInputDefVar "32" "duration"
  fmin <- getInputDefVar "0" "fmin"
  fmax <- getInputDefVar "0" "fmax"
  prevScript <- getInput "prevScript"
  return $ ParamCGI { script = script
                    , message = ""
                    , files = files
                    , lstfile = lstfile
                    , chlist = chlst
                    , gps = gps
                    , locale = locale
                    , channel1 = channel1
                    , channel2 = channel2
                    , monitors = monitors
                    , duration = duration
                    , fmin = fmin
                    , fmax = fmax
                    , prevScript = prevScript
                    }

getInputGPS :: (MonadCGI m) => m ((Maybe String), String)
getInputGPS = do
  date <- getInput "Date"
  case date of
    Just "Local" -> do
      year <- liftM fromJust $ getInput "year"
      month <- liftM fromJust $ getInput "month"
      day <- liftM fromJust $ getInput "day"
      hour <- liftM fromJust $ getInput "hour"
      minute <- liftM fromJust $ getInput "minute"
      second <- liftM fromJust $ getInput "second"
      local <- liftM fromJust $ getInput "local"
      return $ (Just $ time2gps $ year++"-"++month++"-"++day++" "++hour++":"++minute++":"++second++" "++local, local)
    otherwise -> do
      gps <- getInput "gps"
      return (gps, "UTC")

getInputDefVar :: (MonadCGI m) => String -> String -> m String
getInputDefVar defvar tagName = do
  tmp <- getInput tagName
  case tmp of
   Nothing -> return defvar
   Just "" -> return defvar
   Just x  -> return x

{-- HTML Frame --}
-----------------------------------------------------------------------------------------------
------------------------------------- test ----------------------------------------------------
inputFrame :: ParamCGI -> String -> String
inputFrame params x = inputHeader++x++inputFooter
  where inputHeader = "<div style=\"color:#ff0000;\"><b>"++(message params)++"</b></div>"
        inputFooter = ""
------------------------------------- test ----------------------------------------------------
-----------------------------------------------------------------------------------------------
        
resultFrame :: ParamCGI -> String -> String
resultFrame params x = htmlFrame $ errorMsg++resultHeader++x++resultFooter
  where resultHeader = concat ["<h3>GPS Time: "++gps'++"&nbsp; ("++(gps2localTime (read gps') locale')++")</h3>"
                              ,"<p><b>duration: </b>"++(duration params)++"s&emsp;&emsp;"
                              ,"<b>Freq. band: </b>"++(fmin params)++" - "
                               ++(fmaxfN $ fmax params)++" Hz</b>"]
        fmaxfN x = case (read x) == 0.0 of
                    True -> "f<font size=\"1\">Nyquist</font>"
                    False -> x
                    
        resultFooter = cgiNavi params
        gps' = fromJust $ gps params
        locale' = locale params
        errorMsg = "<div style=\"color:#ff0000;\"><b>"++(message params)++"</b></div>"

-----------------------------------------------------------------------------------------------
------------------------------------- test ----------------------------------------------------
resultFrame' :: ParamCGI -> String -> String -> String
resultFrame' params x y = htmlFrame $ errorMsg++resultHeader++x++resultFooter++y
  where resultHeader = concat ["<h3>GPS Time: "++gps'++"&nbsp; ("++(gps2localTime (read gps') locale')++")</h3>"
                              ,"<p><b>duration: </b>"++(duration params)++"s&emsp;&emsp;"
                              ,"<b>Freq. band: </b>"++(fmin params)++" - "
                               ++(fmaxfN $ fmax params)++" Hz</b>"]
        fmaxfN x = case (read x) == 0.0 of
                    True -> "f<font size=\"1\">Nyquist</font>"
                    False -> x
                    
        resultFooter = cgiNavi params
        gps' = fromJust $ gps params
        locale' = locale params
        errorMsg = "<div style=\"color:#ff0000;\"><b>"++(message params)++"</b></div>"
------------------------------------- test ----------------------------------------------------
-----------------------------------------------------------------------------------------------

htmlFrame :: String -> String
htmlFrame x = htmlHeader++x++htmlFooter
  where htmlHeader = "<html><head><title>HasKAL</title></head><body><h1>HasKAL</h1>"
        htmlFooter = concat [
          "<br><Hr><footer>",
          "<div><p>Real time quick look page is <a href=\"../\">here</a><p>",
          "<small>Powerd by <a href=\"https://github.com/gw-analysis\">HasKAL</a></small></footer>"
          ]

cgiNavi :: ParamCGI -> String
cgiNavi params = concat [
  "[<a href=\"", path, "?Date=GPS&gps=", (show $ (read gps') - (read duration')),
  "&duration="++duration', uris, "\">&lt; Prev</a>] ",
  " [<a href=\"", path, "\">Back</a>] ",
  " [<a href=\"", path, "?Date=GPS&gps=", (show $ (read gps') + (read duration')),
  "&duration="++duration', uris, "\">Next &gt;</a>]"
  ]
  where path = script params
        gps' = fromJust $ gps params
        duration' = duration params
        uris = (concat $ zipWith (++) (repeat "&channel1=") $ channel1 params)
               ++ (concat $ zipWith (++) (repeat "&channel2=") $ channel2 params)
               ++ (concat $ zipWith (++) (repeat "&monitor=") $ monitors params)
               ++ "&fmin="++(fmin params) ++ "&fmax="++(fmax params) 

{-- InputForm --}
fileUpForm :: ParamCGI -> String
fileUpForm params = concat [
  "<form action=\"", (script params), "\" method=\"post\" enctype=\"multipart/form-data\">",
  "<h4>upload new channel list file (if you need)</h4>",
  "<input type=\"hidden\" name=\"MAX_FILE_SIZE\" value=\"1\" />",
  "<input type=\"file\" name=\"uploadfile\" accept=\"text/plain\">",
  "<input type=\"submit\" value=\"send\">",
  "</form>"
  ]
             
paramForm :: [MonitorType] -> String
paramForm mons = concat [
  "<div><h3> Parameters: </h3>",
  "<h4>For General</h4>",
  "<p>Duration: <input type=\"text\" name=\"duration\" size=\"5\" /> sec.",
  "&emsp;(default is 32s)</p>",
  "<p>Freq. band: <input type=\"text\" name=\"fmin\" size=\"5\" /> Hz ~ ",
  "<input type=\"text\" name=\"fmax\" size=\"5\" /> Hz",
  "&emsp;(default is from 0Hz to Nyquist freq.)</p>",
  concat $ map eachMon mons,
  "</div>"
  ]
  where eachMon mon
          | elem NHA mons == True = ""
              -- concat ["<h4>For NHA</h4>"
              --        ,"<p># of signal: <input type=\"text\" name=\"nhkchunck\" size=\"5\" />"
              --        ,"&emsp;(default is 4)</p>"
              --        ,"<p>Chunk Size: <input type=\"text\" name=\"nhkchunck\" size=\"5\" /> sec."
              --        ,"&emsp;(default is 0.5s)</p>"
              --        ,"<p>Time resolution: <input type=\"text\" name=\"nhkdt\" size=\"5\" /> sec."
              --        ,"&emsp;(default is 0.0625s)</p>"
              --        ]
          | otherwise = ""


channelForm :: ParamCGI -> [MultiSelect]  -> String
channelForm params flags = concat [
  "<div><h3>Channel List :</h3>",
  "<p><a href=\"generateChannelList.cgi?prevScript="++script params
  ++"\" target=\"input\"> <b>make channel list</b></a></p>",
  "<p><a href=\"selectChannelList.cgi?prevScript="++script params
  ++"\" target=\"input\"> <b>select channel list</b></a> ("++lstfile params++")</p>",
  -- "<table><tr>",
  -- concat $ map (\i -> "<th>Channel "++(show i)++":</th>") [1..length flags],
  -- "</tr><tr>",
  -- concat $ map (\(i, j) ->
  --                "<td><select name=\"channel"++(show i)++"\" size=\"5\""++multi j++" style=\"font-size:90%; \">"
  --                ++(concat $ map (\x -> "<option value=\""++x++"\">"++x++"</option>") $ chlist params)
  --                ++"</select></td>") $ zip [1..length flags] flags,
  -- "</tr></table></div>"]
  concat $ map (\(i, j) ->
                 "Channel "++(show i)++":"
                 ++"<br><select name=\"channel"++(show i)++"\" size=\"5\""++multi j++" style=\"font-size:90%; \">"
                 ++(concat $ map (\x -> "<option value=\""++x++"\">"++x++"</option>") $ chlist params)
                 ++"</select><br>") $ zip [1..length flags] flags
  ]
  where select True = "selected"
        select False = ""
        multi Multi = "multiple"
        multi Single = ""

-----------------------------------------------------------------------------------------------
------------------------------------- test ----------------------------------------------------
monitorForm :: MultiSelect -> [(Bool, MonitorType, String)] -> String
monitorForm x mons = concat [
  "<div><h3> Monitors: </h3>",
  concat $ map (\(c, s, l) -> do
                   "<input type=\""++multi x++"\" name=\"monitor\" value=\""++(show s)++"\" "++chk c++">"++l++"&nbsp") mons,
  "</div>"
  ]
  where chk True = "checked=\"checked\""
        chk False = ""
        multi Single = "radio"
        multi Multi = "checkbox"
------------------------------------- test ----------------------------------------------------
-----------------------------------------------------------------------------------------------

dateForm :: ParamCGI -> String
dateForm params = concat [
  -- "<div><h3> Date: </h3>",
  "<div><fieldset><legend> Date: </legend>",
  "<p><input type=\"radio\" name=\"Date\" value=\"GPS\" checked=\"checked\" />",
  " GPS Time: <input type=\"text\" name=\"gps\" value=\"", fromJust (gps params), "\" size=\"13\" /></p>",
  "<p><input type=\"radio\" name=\"Date\" value=\"Local\" /> Local Time: ", "<br>",
  setDef "year" yr [2015..2020], setDef "month" mon [1..12], setDef "day" day [1..31], "<br>",
  setDef "hour" hrs [0..23], ":", setDef "minute" min [0..59], ":", setDef "second" sec [0..59], "&ensp;",
  "<select name=\"local\">",
  concat $ map (\x -> "<option value=\""++x++"\" >"++x++"</option>") ["JST", "UTC"],
  "</select></p>"
  -- , "<p>test type date</p>"
  -- ,"<input type=\"date\" name=\"date\">"
  ,"</fieldset>"
  ]
  where (yr, mon, day, hrs, min, sec, _) = gps2localTimetuple (read $ fromJust $ gps params) "JST"

{-- Result Body --}
-- For display PNG 
genePngTable :: [(Message, String, [String])] -> String
genePngTable filenames = concat $ map genePngTableCore filenames

genePngTableCore :: (Message, String, [String]) -> String
genePngTableCore (msg, ch, files) = concat ["<h3>"++ch++msg'++"</h3>"
                                           ,errhtml++"<br>"++body++"<br clear=\"top\"><Hr>"]
  where (msg', body) = case (isPrefixOf "ERROR" msg) of
                True  -> ("<span style=\"color: #ff0000;\"> "++msg++"</span>", "")
                False -> ("<span>&ensp; (fs="++msg++" Hz)</span>"
                         , concat $ newline 4 $ map imghtml $ filter (not . isPrefixOf "ERROR") files)
        errhtml = concat $ map (\x -> "<p style=\"color: #ff0000;\">"++x++"</p>") $ filter (isPrefixOf "ERROR") files
        imghtml x = "<nobr><a target=\"_blank\" href=\""++x++"\">"++"<img alt=\"\" src=\""++x
                  ++"\" align=\"top\" style=\"border: 0px solid; width: 300px;\"></a></nobr>"
        newline n xs = map ((++"<br><br>").concat.(\i -> (take n).(drop $ i*n) $ xs)) [0,1..length xs`div`n]


-- For Peason and MIC
geneChMap :: ParamCGI -> [(Message, String, [String])] -> String
geneChMap params x = concat [
  "<table border=\"1\" cellpadding=\"6\"><tr bgcolor=\"dddddd\"><th></th>",
  concat $ map (\(_, y, _) -> "<th>"++y++"</th>") x,
  concat $ map (geneChMapCore params chs) x,
  "</table>"]
  where chs = map (\(_, b, _) -> b) x

geneChMapCore :: ParamCGI -> [String] -> (Message, String, [String]) -> String
geneChMapCore params chs (msg, ch1, xs) = result
  where result = case (msg=="") of
                  True -> concat [
                    "<tr><th bgcolor=\"#eeeeee\">"++ch1++"</th>",
                    concat $ zipWith (\x y-> "<td bgcolor="++(color x)++"><a href=\""++(url ch1 y)
                                        ++"\" target=\"_blank\">"++(showFFloat (Just 5) x "")++"</td>") xs' chs,
                    "</tr>"]
                  False -> ""
        url ch1 ch2 = "./date_2.cgi?Date=GPS&gps="++(fromJust $ gps params)++"&duration="++(duration params)++"&channel1="
                       ++ch1++"&channel2="++ch2++"&monitor="++(head $ monitors params)
                       ++"&fmin="++(fmin params)++"&fmax="++(fmax params)
        xs' = map read xs :: [Double]
        color val | val > 0.8    = "\"#ff5555\""
                  | val > 0.6    = "\"#ffaaaa\""
                  | val > 0.4    = "\"#ffeeee\""
                  | val > (-0.4) = "\"#ffffff\""
                  | val > (-0.6) = "\"#eeeeff\""
                  | val > (-0.8) = "\"#aaaaff\""
                  | otherwise    = "\"#5555ff\""

-- For Bruco
geneRankTable :: ParamCGI -> [(Double, [(Double, String)])] -> String 
geneRankTable params xs = concat [
  "<h3>Channel: "++(head $ channel1 params)++"</h3>",
  "<table cellspacing=\"10\"><tr>",
  concat $ map (\n -> "<th><nobr>"++(show.fst.head $ drop (len*n) xs)++"Hz~</nobr></th>") [0..(num-1)],
  "</tr><tr>",
  concat $ map (\n -> concat [
                   "<td><table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"font-size:3px;\">",
                   "<tr bgcolor=\"cccccc\"><th>freq. [Hz]</th>",
                   concat $ map nthLabel [1..num],
                   concat $ map (geneRankTableCore params num) $ take len $ drop (len*n) xs,
                   "</table></td>"]) [0..(num-1)],
  "</tr></table>"]
  where len = length xs `div` num
        num = 5
        nthLabel n = "<th>"++(show n)++"th ch.</th>"

geneRankTableCore :: ParamCGI -> Int -> (Double, [(Double, String)]) -> String
geneRankTableCore params n (freq, res) = concat [
  "<tr><th bgcolor=\"#cccccc\"><nobr>"++(show freq)++" Hz&emsp;</nobr></th>",
  concat.(take n') $ map (\(val, ch) -> "<td bgcolor="++(color val)++"><nobr><a href=\""++url freq ch
                                        ++"\" target=\"_blank\">"++ch++"</a>&emsp;</nobr><br>") res,
  "</tr>"]
  where color val | val > 0.8 = "\"#ff5555\""
                  | val > 0.6 = "\"#ffaaaa\""
                  | val > 0.4 = "\"#ffeeee\""
                  | otherwise = "\"#ffffff\""
        n' = min n (length res)
        url freq ch2 = "./date_2.cgi?Date=GPS&gps="++(fromJust $ gps params)++"&duration="++(duration params)++"&channel1="
                       ++(head $ channel1 params)++"&channel2="++ch2++"&monitor="++(head $ monitors params)
                       ++"&fmin="++(show $freq-10)++"&fmax="++(show $freq+10)

{--  supplement function --}
setDef :: String -> Int -> [Int] -> String
setDef name x ys = concat [
  "<select name=\""++name++"\">"
  ,(concat.(`map` ys) $ \y -> do
       let (v, w) = show0 name y
       case x==y of True -> "<option value=\""++v++"\" selected>"++w++"</option>"
                    False -> "<option value=\""++v++"\">"++w++"</option>")
  ,"</select>"
  ]

show0 :: String -> Int -> (String, String)
show0 "month" n
  | n==1 = ("01", "Jan.")
  | n==2 = ("02", "Feb.")
  | n==3 = ("03", "Mar.")
  | n==4 = ("04", "Apr.")
  | n==5 = ("05", "May")
  | n==6 = ("06", "Jun.")
  | n==7 = ("07", "Jul.")
  | n==8 = ("08", "Aug.")
  | n==9 = ("09", "Sep.")
  | n==10 = ("10", "Oct.")
  | n==11 = ("11", "Nov.")
  | n==12 = ("12", "Dec.")
  | otherwise = ("00", "???")
show0 _ n
  | len==1 = ("0"++(show n), show n)
  | len>=2 = (show n, show n)
  where len = length $ (show n)
