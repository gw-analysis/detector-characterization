module HasKAL.PlotUtils.PlotOption.PlotOptionHROOT(
       PlotTypeOption(Line, Point, LinePoint, PointLine, Dot),
       PlotTypeOption3D(COLZ, CONTZ, LEGO2Z, AITOFF, MERCATOR),
       LogOption(Linear, LogX, LogY, LogZ, LogXY, LogXZ, LogYZ, LogXYZ),
       MultiPlot(Over, Divide),
       ColorOpt(BLACK, RED, GREEN, BLUE, YELLOW, PINK, CYAN),
       ) where

data PlotTypeOption = Line | Point | LinePoint | PointLine | Dot deriving Eq
data LogOption = Linear | LogX | LogY | LogZ | LogXY | LogXZ | LogYZ | LogXYZ deriving Eq
data PlotTypeOption3D = COLZ | CONTZ | LEGO2Z | AITOFF | MERCATOR deriving (Eq, Show)
data MultiPlot = Over | Divide deriving (Eq)
data ColorOpt = BLACK | RED | GREEN | BLUE | YELLOW | PINK | CYAN deriving (Eq, Ord) -- 未実装
