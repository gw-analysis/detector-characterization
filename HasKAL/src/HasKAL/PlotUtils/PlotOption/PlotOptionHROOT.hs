


module HasKAL.PlotUtils.PlotOption.PlotOptionHROOT(
       PlotTypeOption(Line, Point, LinePoint, PointLine, Dot),
       PlotTypeOption3D(COLZ, CONTZ, LEGO2Z, AITOFF, MERCATOR),
       LogOption(Linear, LogX, LogY, LogZ, LogXY, LogXZ, LogYZ, LogXYZ),
       MultiPlot(Over, Divide),
       ColorOpt(WHITE, BLACK, RED, GREEN, BLUE, YELLOW, PINK, CYAN),
       ) where

data PlotTypeOption = Line | Point | LinePoint | PointLine | Dot deriving Eq
data LogOption = Linear | LogX | LogY | LogZ | LogXY | LogXZ | LogYZ | LogXYZ deriving (Eq, Read, Show)
data MultiPlot = Over | Divide deriving (Eq)

data PlotTypeOption3D = COLZ | CONTZ | LEGO2Z | AITOFF | MERCATOR deriving (Eq, Show)
data ColorOpt = WHITE | BLACK | RED | GREEN | BLUE | YELLOW | PINK | CYAN deriving (Eq, Ord, Show, Read)
