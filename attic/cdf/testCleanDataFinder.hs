



module CleanDataFinder
where

import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as V
import HasKAL.LineUtils.LineRemoval.RngMedian (rngMedV)



data CutoffType = Low | High deriving (Show)




-- | Noise Floor Estimation
nfEstimate :: Int
           -> (Double, Double)
           -> (V.Vector Double, V.Vector Double)
           -> Maybe (V.Vector Double, V.Vector Double)
nfEstimate blcksz (fl, fu) psddat = do
  let (fv', dat') = psddat
      maybeflind = findCutoffInd fv' fl Low
      maybefuind = findCutoffInd fv' fu High
  case (maybeflind, maybefuind) of
        (Just flind', Just fuind') -> do let (flind, fuind) = (fst flind', fst fuind')
                                             dat = V.drop flind $ V.take fuind dat'
                                             fv  = V.drop flind $ V.take fuind fv'
                                         Just (fv, rngMedV dat (V.length dat) blcksz)
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing




findCutoffInd :: V.Vector Double
              -> Double
              -> CutoffType
              -> Maybe (Int, Double)
findCutoffInd input x0 cuttype =
  case cuttype of
    Low -> ascend 0
    High-> descend (V.length input)
  where ascend m = if x0 <= input!m
                     then Just (m, input!m)
                     else if m+1 > V.length input
                            then Nothing
                            else ascend (m+1)
        descend m = if x0 >= input!m
                      then Just (m, input!m)
                      else if m-1 < 1
                             then Nothing
                             else descend (m-1)

