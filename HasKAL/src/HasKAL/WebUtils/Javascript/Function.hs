
module HasKAL.WebUtils.Javascript.Function (
  expandFont,
  getFrameURL,
  mailtoURL
) where


expandFont :: Int -> Int -> String -> String
expandFont defPx shiftPx className = jsFrame defJS body
  where defJS = concat [
          "var x = "++(show defPx)++";",
          "function larger()  {",
            "x += "++(show shiftPx)++";",
            "var y = document.getElementsByClassName('"++(className)++"');",
            "for (var i = 0; i < y.length; i++){",
              "y[i].style.fontSize = x + 'px';",
            "}",
          "}",
          "function smaller()  {",
            "x -= "++(show shiftPx)++";",
            "var y = document.getElementsByClassName('"++(className)++"');",
            "for (var i = 0; i < y.length; i++){",
              "y[i].style.fontSize = x + 'px';",
            "}",
          "}"
          ]
        body = concat [
          "<div>",
          "<span>font size: </span>",
          "<input type=\"submit\" value=\"-\" onclick=\"smaller();\">",
          "<input type=\"submit\" value=\"+\" onclick=\"larger();\">",
          "</div>"
          ]

getFrameURL :: String
getFrameURL = jsFrame defJS body
  where defJS = concat [
          "function alertURL () {",
            "var url = document.location.href;",
            "window.prompt(\"URL of these results.\",url);",
          "}"
          ]
        body = concat [
          "<span>",
          "<input type=\"submit\" value=\"Get URL\" onclick=\"alertURL();\" style=\"font-size:16px\">",
          "</span>"
          ]

mailtoURL :: String
mailtoURL = jsFrame defJS body
  where defJS = concat [
          "function mailtoURL (){",
          "var url = document.location.href;",
          "window.open('mailto:?body='+url);",
          "}"
          ]
        body = concat [
          "<span>",
          "<input type=\"submit\" value=\"Send URL\" onclick=\"mailtoURL();\" style=\"font-size:16px\">",
          "</span>"
          ]

{-- Internal Functions --}
jsFrame :: String -> String -> String
jsFrame defJS body = "<script language=\"Javascript\"><!--\n"++defJS++"\n--></script>"++body
