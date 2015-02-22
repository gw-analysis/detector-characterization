

{- to run the code,
- ghc learnDiagrams.hs
- ./learnDiagrams -o chart.svg -w 800
-}

import Diagrams.Prelude
import Graphics.SVGFonts.ReadFont
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Core.Points()

main = mainWith example

type Points=[(Double, Double)]

dataSeries :: [(String, Points)]
dataSeries =
  [ ("upward",   zip [0.0, 1.0 .. 10.0] [0.0, 1.0 .. 10.0])
  , ("downward", zip [0.0, 1.0 .. 10.0] [10.0, 9.0  .. 0.0])
  , ("cycle",    zip [0.0, 1.0 .. 10.0] (cycle [3, 4, 5]))
  , ("arbitrary",[(2, 4), (4, 2), (5, 4), (10, 5)])
  , ("sin",      map (\x->(x, 8+sin x)) [0.0, 0.5 .. 10.0])
  ]

type Dia = Diagram B R2

example :: Dia
example = centerXY $
  (centerY (chart (map snd dataSeries) plotStyles [0, 2, 4, 6, 8, 10] [0, 2, 4, 6, 8, 10])
   ||| strutX 1
   ||| centerY (legend plotStyles (map fst dataSeries)))
   `atop` square 12 # translateX 5 # scaleY 0.85 -- border


h, w :: Double
h = 7
w = 7

chart  :: [Points] -> [(Dia, Dia->Dia)] -> [Double] -> [Double] -> Dia
chart series styles xs ys = mconcat
  [ plotMany styles series dataToFrac
  , horizticks (map (\x->((x-minx)/xrange, showFloor x)) xs)
  , vertticks  (map (\y->((y-miny)/yrange, showFloor y)) ys)
  , box
  ]
  where
    maxx = last xs
    minx = head xs
    maxy = last ys
    miny = head ys
    xrange = maxx-minx
    yrange = maxy-miny
    dataToFrac (x, y) = ((x-minx)/xrange, (y-miny)/yrange)
    showFloor = show . (floor :: Double->Integer)

plot :: ((Double, Double)->(Double, Double))->Dia->(Dia->Dia)->[(Double, Double)]->Dia
plot dataToFrac shape lineStyle ps =
    let scalify (x, y) = (x*w, y*h)
        ps' = map (p2 . scalify .dataToFrac) ps
     in (stroke $ fromVertices ps') # lineStyle
          `beneath` mconcat [ shape # moveTo p | p <- ps']

plotMany :: [(Dia, Dia->Dia)]->[[(Double, Double)]]->((Double, Double)->(Double, Double))->Dia
plotMany styles seriesList dataToFrac =
  mconcat $ zipWith (uncurry (plot dataToFrac)) (styles++plotStyles) seriesList

text' :: String->Dia
text' s = (stroke $ textSVG' (TextOpts s lin2 INSIDE_H KERN False 0.4 0.4)) # fc black # lw none

legend :: [(Dia, Dia->Dia)]->[String]->Dia
legend styles labels = centerXY $
  vcat' with {_sep=0.15} $
    map (\(l, s)->littleLine s ||| strutX 0.4 ||| text' l # alignL)
      (zip labels (styles ++ plotStyles))
  where littleLine (d, l) = (stroke $ fromVertices [ 0^&0, 1^&0 ]) # l
                           <> d # moveTo (0.5^&0)

box :: Dia
box = strokeLoop . closeLine . fromVertices $ [0^&0, 0^&h, w^&0]

vertticks :: [(Double, String)]->Dia
vertticks pairs =
  let textBits = mconcat [text' t # alignR # moveTo ((-0.2)^&(y*h)) | (y, t)<-pairs]
      tickBits =   mconcat [fromVertices [0^&(y*h), 0.1^&(y*h)] | (y, _)<-pairs]
                <> mconcat [fromVertices [w^&(y*h), (w-0.1)^&(y*h)] | (y, _)<-pairs]
                <> mconcat [fromVertices [0^&(y*h), w^&(y*h)] # lc gray # dashingG [0.1, 0.1] 0
                           | (y, _)<-pairs]
   in textBits <> tickBits

horizticks :: [(Double, String)]->Dia
horizticks pairs =
  let textBits = mconcat [text' t # moveTo ((x*w)^&(-0.3)) | (x, t)<-pairs]
      tickBits =   mconcat [fromVertices [(x*w)^&0, (x*w)^&0.1] | (x, _)<-pairs]
                <> mconcat [fromVertices [(x*w)^&h, (x*w)^&(h-0.1)] | (x, _)<-pairs]
                <> mconcat [fromVertices [(x*w)^&0, (x*w)^&h] # lc gray # dashingG [0.1, 0.1] 0
                           | (x, _)<-pairs]
   in textBits <> tickBits

newtype Fill = Fill Bool
type Shape = Dia
type DotStyle = (Shape, Fill)
type LineStyle = Dia->Dia

plotStyles :: [(Shape, LineStyle)]
plotStyles = zipWith3 combineStyles dotStyles colourStyles lineStyles

combineStyles :: DotStyle->Colour Double-> LineStyle->(Shape, LineStyle)
combineStyles (d, Fill f) c l =
  (d # (if f then fcA (c `withOpacity` 0.5) else id) # lc c, lc c .l)

dotStyles :: [DotStyle]
dotStyles = cycle $
  let shapes = map (stroke)
        [ circle 0.07
        , square 0.1
        , eqTriangle 0.1
        , pentagon 0.1
        , cross 0.07
        , plus 0.07
        , star (StarSkip 2) (pentagon 0.1)
        ]
   in [(s, Fill b) | b<-[True, False], s<-shapes]

cross :: Double->Path R2
cross x = fromVertices [x^&(-x), ((-x)^&x)]
          <> fromVertices [x^&x, ((-x)^&(-x))]

plus :: Double->Path R2
plus x = cross x # rotate (45 @@ deg)

colourStyles :: [Colour Double]
colourStyles = cycle $ [red, green, blue, brown]

lineStyles :: [Dia->Dia]
lineStyles = cycle $
               [ id
               , dashingG [0.1, 0.1] 0
               , dashingG [0.02, 0.02] 0
               , dashingG [0.1, 0.1, 0.03, 0.1] 0
               , dashingG [0.1, 0.1, 0.02, 0.02, 0.02, 0.1] 0
               ]




