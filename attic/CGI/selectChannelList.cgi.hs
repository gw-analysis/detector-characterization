
import Network.CGI
import Data.List (isInfixOf, isSuffixOf, isPrefixOf, nub, foldl')
import Data.Maybe (fromJust)

import HasKAL.WebUtils.CGI.Function
-- import HasKAL.DataBaseUtils.XEndEnv.Function (kagraChannelList)
import SampleChannel

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  params <- getInputParams defaultChs
  output $ listPage params 

listPage :: ParamCGI -> String
listPage params = inputFrame params body
  where body = concat [
          "<form action=\"", origScript params, "\" method=\"GET\" target=\""++target++"\">",
          geneListBox params,
          "<input type=\"hidden\" name=\"prevScript\" value=\""++(fromJust $ prevScript params)++"\" />",
          "<div><input type=\"submit\" value=\"select\" /></div>",
          "</form>"]

origScript :: ParamCGI -> String
origScript params =
  case prevScript params of
   Nothing -> "date_1.cgi"
   Just "" -> "date_1.cgi"
   Just x  -> x

target="input" -- 入力フォームのあったフレーム

geneListBox :: ParamCGI -> String
geneListBox params = concat [
  "<div><h3> Channels List Files: </h3>",
  "<div style=\"overflow:scroll; width:100%; height:500px; background-color:#eeeeee\">",
  "<p><input type=\"radio\" name=\"lstfile\" value=\"Default\" checked=\"checked\"> Default&nbsp</p>",
  concat $ map (\x -> "<p><input type=\"radio\" name=\"lstfile\" value=\""++x++"\" > "++x++"&nbsp</p>") $ filter (isSuffixOf ".txt") (files params),
  "</div>",
  "</div>"]
