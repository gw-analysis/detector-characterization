

import Network.CGI

main :: IO ()
main = runCGI (handleErrors cgiMain)


cgiMain :: CGI CGIResult
cgiMain = do
  script <- scriptName
  x <- getInput "myTag"
  output $ fork script x

fork :: String -> Maybe String -> String
fork script x =
  case x of
   Nothing -> htmlbody script
   Just "" -> htmlbody script
   Just y  -> showResult y

htmlbody :: String -> String
htmlbody script = "<html><head><title>HOGE</title></head><body><form action=\""++script++"\" method=\"GET\">"
                  ++"<h1>Please input something</h1><input type=\"text\" name=\"myTag\" /><input type=\"submit\" value=\"submit\" />"
                  ++"</form></body></html>"

{-- showResult --}{--
<html>
  <head>
    <title>HOGE</title>
  </head>
  <body>
    <form action="@@ script @@" method="GET">
    <h1>Please input something</h1>
    <input type="text" name="myTag" />
    <input type="submit" value="submit" />
    </form>
  </body>
</html>
--}

showResult :: String -> String
showResult x = "<html><head><title>FUGA</title></head><body><h1>Input String is</h1>"++x++"</body></html>"

{-- showResult --}{--
<html>
  <head>
    <title>FUGA</title>
  </head>
  <body>
    <h1>Input String is</h1>
     @@ Something @@
  </body>
</html>
--}
