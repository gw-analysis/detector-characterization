

import Network.CGI

main :: IO ()
main = runCGI (handleErrors cgiMain)


cgiMain :: CGI CGIResult
cgiMain = do
  output $ htmlbody

htmlbody :: String
htmlbody = "<html><head><title>HOGE</title></head><body><h1>hoge</h1>hogehoge</body></html>"


{-- htmlbody --}{--
<html>
  <head>
    <title>HOGE</title>
  </head>
  <body>
    <h1>hoge</h1>
    hogehoge
  </body>
</html>
--}
