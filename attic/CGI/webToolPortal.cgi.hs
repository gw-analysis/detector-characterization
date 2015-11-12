
import Network.CGI

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  portal <- getInput "portal"
  output $ html portal

html :: Maybe String -> String
html portal = concat [
  "<html>",
  "<head>",
  "<title>HasKAL Web Monitor</title>",
  "<base target=\"_top\">",
  "</head>",
  "<body>",
  "<div align=center>",
  "<table border=0 cellspacing=0 cellpadding=0>",
  "<tr>",
  concat $ map (subhtml portal) [
    "Single Channel Analysis",
    "Coherence Analysis",
    "Correlation Map",
    "Bruco",
    "Detection Range"],
  "<td align=center valign=middle width=98 height=30>",
  "<div align=center><a href=\"dailyFrame.cgi\">Daily Summary page</a></div>",
  "</td>",
  "</tr>",
  "</table>",
  "</div>",
  "</body>",
  "</html>"
  ]

subhtml :: Maybe String -> String -> String
subhtml portal sys = concat [
  "<td align=center valign=middle width=98 height=30 "++color portal sys++">",
  "<div align=center><a href=\"./webToolFrame.cgi?portal="++sys++"\">"++sys++"</a></div>",
  "</td>"
  ]
  where color portal sys = case portal of
                            Just x -> case x==sys of
                                       True -> "bgcolor=\"#ccffff\""
                                       False -> ""
                            Nothing -> case sys=="Single Channel Analysis" of
                                        True -> "bgcolor=\"#ccffff\""
                                        False -> ""

