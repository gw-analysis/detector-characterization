{- learning diagrams
- http://projects.haskell.org/diagrams/doc/cmdline.html
- 5 Clock Example
-}

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
--import Diagrams.Coordinates
import Data.Time
import Diagrams.Animation

clock :: UTCTime -> Diagram B R2
clock t = circle 0.35 # fc silver # lwG 0
       <> bigHand # f 12 h <> littleHand # f 60 m
       <> circle 1 # fc black # lwG 0
       <> circle 11 # lwG 1.5 # lc slategray # fc lightsteelblue
  where
  s = realToFrac $ utctDayTime t :: Double
  m = s / 60
  h = m / 60
  bigHand = (0 ^& (-1.5)) ~~ (0 ^& 7.5) # lwG 0.5
  littleHand = (0 ^& (-2)) ~~ (0 ^& 9.5) # lwG 0.2
  f n v = rotate (- v / n @@ turn)

main = mainWith (clock <$> getCurrentTime)




