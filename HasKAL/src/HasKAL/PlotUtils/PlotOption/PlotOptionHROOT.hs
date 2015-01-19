{- |
Module      : HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

plot option

- [1] http://root.cern.ch/root/html/THistPainter.html#HP01
- [2] http://root.cern.ch/root/html/TAttFill.html#F1
-}
module HasKAL.PlotUtils.PlotOption.PlotOptionHROOT(
       PlotTypeOption(Line, Point, LinePoint, PointLine, Dot),
       PlotTypeOption3D(COLZ, CONTZ, LEGO2Z, AITOFF, MERCATOR),
       LogOption(Linear, LogX, LogY, LogZ, LogXY, LogXZ, LogYZ, LogXYZ),
       MultiPlot(Over, Divide),
       ColorOpt(BLACK, RED, GREEN, BLUE, YELLOW, PINK, CYAN),
       ) where

data PlotTypeOption = Line | Point | LinePoint | PointLine | Dot deriving Eq
data LogOption = Linear | LogX | LogY | LogZ | LogXY | LogXZ | LogYZ | LogXYZ deriving Eq
data MultiPlot = Over | Divide deriving (Eq)

data PlotTypeOption3D = COLZ | CONTZ | LEGO2Z | AITOFF | MERCATOR deriving (Eq, Show)
data ColorOpt = BLACK | RED | GREEN | BLUE | YELLOW | PINK | CYAN deriving (Eq, Ord)
