module HasKAL.PlotUtils.PlotOption.PlotOptionHROOT(
       PlotTypeOption(Line, Point, LinePoint, PointLine, Dot),
       LogOption(Linear, LogX, LogY, LogXY),
       ) where

data PlotTypeOption = Line | Point | LinePoint | PointLine | Dot deriving Eq
data LogOption = Linear | LogX | LogY | LogXY deriving Eq
