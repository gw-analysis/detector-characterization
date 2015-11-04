
import Network.CGI
import Data.List (isInfixOf)

import HasKAL.WebUtils.CGI.Function
import HasKAL.DataBaseUtils.Function (kagraChannelList)

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  params <- getInputParams
  flag <- getInput "ch_flag"
  words <- getInput "keyword"
  oName <- getInput "outputname"
  str <- liftIO $ fork params flag words oName
  output $ str 

fork :: ParamCGI -> Maybe String -> Maybe String -> Maybe String -> IO String
fork params flag words oName = do
  nowGps <- return $ show 1120543424 -- getCurrentGps 
  case flag of
   Nothing -> do
     case (gps params, words) of
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
           let list' = filter (isInfixOf y) list :: [String]
           return $ listPage params list'
   Just "" -> do
     case (gps params, words) of
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
           let list' = filter (isInfixOf y) list :: [String]
           return $ listPage params list'
   Just "1" -> do
     case (channel1 params, oName) of
      ([], _) -> return $ inputForm params
      (xs, Nothing) -> return $ listPage params $ xs
      (xs, Just "") -> return $ listPage params $ xs
      (xs, Just y) -> do
        writeFile ("../ch_list/"++y++".txt") (unlines xs)
        return $ finalPage params

   
inputForm :: ParamCGI -> String
inputForm params = inputFrame params formbody
  where formbody = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\""++target++"\">",
          (dateForm params),
          "<p>keyword: <input type=\"text\" name=\"keyword\" size=\"15\" /></p>",
          "<div><input type=\"submit\" value=\"channel search\" /></div>",
          "</form>"]

listPage :: ParamCGI -> [String] -> String
listPage params list = inputFrame params body
  where body = concat [
          "<form action=\"", (script params), "\" method=\"GET\" target=\""++target++"\">",
          "<input type=\"hidden\" name=\"ch_flag\" value=\"1\" />",
          geneListBox list,
          "<p>file name: <input type=\"text\" name=\"outputname\" size=\"15\" /></p>",
          "<div><input type=\"submit\" value=\"generate\" /></div>",
          "</form>"]

finalPage :: ParamCGI -> String
finalPage params = inputFrame params body
  where body = concat [
          "<h2>Success full<h2>",
          "<form action=\"", script_orig, "\" method=\"GET\" target=\""++target++"\">",
          "<div><input type=\"submit\" value=\"return\" /></div>",
          "</form>"]

script_orig ="date_1.cgi" -- 元いた入力フォームの名前
target="input" -- 入力フォームのあったフレーム

geneListBox :: [String] -> String
geneListBox list = concat [
  "<div><h3> Hit Channels: </h3>",
  concat $ map (\x -> "<p><input type=\"checkbox\" name=\"channel1\" value=\""++x++"\" checked=\"checked\">"++x++"&nbsp</p>") list,
  "</div>"]
