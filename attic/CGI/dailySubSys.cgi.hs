import Network.CGI

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  subSys <- getInput "subSys"
  output $ html subSys

html :: Maybe String -> String
html subSys = concat [
  "<html>",
  "<head>",
  "<title>HasKAL Daily Summary</title>",
  "<base target=\"_top\">",
  "</head>",
  "<body>",
  "<div align=center>",
  "<table border=0 cellspacing=0 cellpadding=0>",
  "<tr>",
  concat $ map (subhtml subSys) [{--"TUN", "FCL", "VAC", "CRY", --}
    "VIS", {--"MIR", "LAS", "MIF",--}
    "IOO"{--, "AOS", "AEL", "DGS",--}
    {--"DMG", "DAS", "GIF", "DEC",--}],
  "<td align=center valign=middle width=98 height=30>",
  "<div align=center><a href=\"./main_frame_1.html\">Web Tools</a></div>",
  "</td>",
  "</tr>",
  "</table>",
  "</div>",
  "</body>",
  "</html>"
  ]

subhtml :: Maybe String -> String -> String
subhtml subSys sys = concat [
  "<td align=center valign=middle width=98 height=30 "++color subSys sys++">",
  "<div align=center><a href=\"./dailyFrame.cgi?subSys="++sys++"\">"++sys++"</a></div>",
  "</td>"
  ]
  where color subSys sys = case subSys of
                            Just x -> case x==sys of
                                       True -> "bgcolor=\"#ccffff\""
                                       False -> ""
                            Nothing -> ""
                                         
