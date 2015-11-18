
import Network.CGI
import Data.List (isInfixOf, isPrefixOf, nub, foldl')
import Data.Maybe (fromJust)

import HasKAL.WebUtils.CGI.Function
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraChannelList)
import SampleChannel

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  params <- getInputParams defaultChs
  flag <- getInput "ch_flag"
  word <- getInput "keyword"
  oName <- getInput "outputname"
  str <- liftIO $ fork params flag word oName
  output $ str 

fork :: ParamCGI -> Maybe String -> Maybe String -> Maybe String -> IO String
fork params flag word oName = do
  nowGps <- return $ show 1120543424 -- getCurrentGps 
  case flag of
   Nothing -> do
     case (gps params, word) of
      (Nothing, _) -> return $ inputForm $ updateGps nowGps params
      (Just "", _) -> return $ inputForm $ updateGps nowGps params
      (_, Nothing) -> return $ inputForm params
      (_, Just "") -> return $ inputForm params
      (Just x, Just y) -> do
        clist <- kagraChannelList (read x) 
        case clist of
         Nothing -> return $ inputForm params
         Just [] -> return $ inputForm params
         Just list -> do
           let keys = words y
           let allow = filter (not.isPrefixOf "-") keys
               list' = nub.concat $ map (\x -> filter (isInfixOf x) list) allow
           let deny = map (drop 1) $ filter (isPrefixOf "-") keys
               list'' = foldl' (\xs y -> filter (not.isInfixOf y) xs) list' deny
           return $ listPage params list''
   Just "" -> do
     case (gps params, word) of
      (Nothing, _) -> return $ inputForm $ updateGps nowGps params
      (Just "", _) -> return $ inputForm $ updateGps nowGps params
      (_, Nothing) -> return $ inputForm params
      (_, Just "") -> return $ inputForm params
      (Just x, Just y) -> do
        clist <- kagraChannelList (read x) 
        case clist of
         Nothing -> return $ inputForm params
         Just [] -> return $ inputForm params
         Just list -> do
           let keys = words y
           let allow = filter (not.isPrefixOf "-") keys
               list' = nub.concat $ map (\x -> filter (isInfixOf x) list) allow
           let deny = map (drop 1) $ filter (isPrefixOf "-") keys
               list'' = foldl' (\xs y -> filter (not.isInfixOf y) xs) list' deny
           return $ listPage params list''
   Just "1" -> do
     case (channel1 params, oName) of
      ([], _) -> return $ inputForm params
      (xs, Nothing) -> return $ listPage params $ xs
      (xs, Just "") -> return $ listPage params $ xs
      (xs, Just y) -> do
        writeFile (chlistDir++y++".txt") (unlines xs)
        return $ finalPage params

   
inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\""++target++"\">",
          (dateForm params),
          "<p>keyword: <input type=\"text\" name=\"keyword\" size=\"30\" /></p>",
          "<input type=\"hidden\" name=\"prevScript\" value=\""++(fromJust $ prevScript params)++"\" />",
          "<div><input type=\"submit\" value=\"channel search\" /></div>",
          "</form>"]

listPage :: ParamCGI -> [String] -> String
listPage params list = inputFrame params body
  where body = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\""++target++"\">",
          "<input type=\"hidden\" name=\"ch_flag\" value=\"1\" />",
          geneListBox list,
          "<p>output name: <input type=\"text\" name=\"outputname\" size=\"30\" /></p>",
          "<input type=\"hidden\" name=\"prevScript\" value=\""++(fromJust $ prevScript params)++"\" />",
          "<div><input type=\"submit\" value=\"generate\" /></div>",
          "</form>"]

finalPage :: ParamCGI -> String
finalPage params = inputFrame params body
  where body = concat [
          "<h2>Success full</h2>",
          "<form action=\"", origScript params, "\" method=\"GET\" target=\""++target++"\">",
          "<input type=\"hidden\" name=\"prevScript\" value=\""++(fromJust $ prevScript params)++"\" />",
          "<div><input type=\"submit\" value=\"return\" /></div>",
          "</form>"]

origScript :: ParamCGI -> String
origScript params =
  case prevScript params of
   Nothing -> "date_1.cgi"
   Just "" -> "date_1.cgi"
   Just x  -> x

target="input" -- 入力フォームのあったフレーム

geneListBox :: [String] -> String
geneListBox list = concat [
  "<div><h3> Hit Channels: </h3>",
  "<div style=\"overflow:scroll; width:100%; height:500px; background-color:#eeeeee\">",
  concat $ map (\x -> "<p><input type=\"checkbox\" name=\"channel1\" value=\""++x++"\" checked=\"checked\"> "++x++"&nbsp</p>") list,
  "</div>",
  "</div>"]
