


module HasKAL.SignalProcessingUtils.Cascade
( tf2cascade
) where


import Numeric.LinearAlgebra
import Numeric.GSL.Polynomials(polySolve)
import Data.List
import Data.Maybe (fromJust)


tf2cascade :: ([Double], [Double]) -> [([Double], [Double])]
tf2cascade (num, denom) =
  let zeroz = polySolve $ reverse num
      polez = polySolve $ reverse denom
      polec' = [x | x <- polez, imagPart x>0]
      poler' = [x | x <- polez, imagPart x==0]
      zeroc = [x | x <- zeroz, imagPart x>0]
      zeror = [x | x <- zeroz, imagPart x==0]
      polec = reverse . cmplxSort $ polec'
      poler = reverse . cmplxSort $ poler'
      secDenom = fsecDenom polec
      secNum = fsecNum polec zeroc zeror
   in out poler zeror secNum secDenom
   where
     out poler zeror secNum secDenom
      | null poler = zip secNum secDenom
      | length poler == 1 = zip secNum secDenom
        ++ [([1, -realPart (last zeror), 0], [1, -realPart (last poler), 0])]
      | length poler == 2 = zip secNum secDenom
        ++ [([1, -realPart (last poler)-realPart (last.init $ poler)
           , realPart (last poler)*realPart (last.init  $ poler)]
           , [1, -realPart (last zeror)-realPart (last.init $ zeror)
           , realPart (last zeror)*realPart (last.init $ zeror)])]
      | otherwise = error "something wrong,  perhaps you have >2 real zero"


cmplxSort :: [Complex Double] -> [Complex Double]
cmplxSort [] = []
cmplxSort (r:+c:xs) = cmplxSort lt ++ [r:+c] ++ cmplxSort gteq
                        where
                          lt = [r':+c' | r':+c' <- xs, abs r' < abs r]
                          gteq=[r':+c' | r':+c' <- xs, abs r' >= abs r]


fsecDenom :: [Complex Double] -> [[Double]]
fsecDenom [] = []
fsecDenom polec = [1, -2*realPart (head polec), realPart (abs (head polec))**2.0] : fsecDenom (tail polec)


fsecNum :: [Complex Double] -> [Complex Double] -> [Complex Double] -> [[Double]]
fsecNum [] _ _ = []
fsecNum polec zeroc zeror =
  case length zeroc >0 of
    True -> let a = map (\i-> realPart (abs (head polec - i))) zeroc
                mina = minimum a
                ind = fromJust $ elemIndex mina a
             in [1, -2*realPart (zeroc!!ind), realPart (abs (zeroc!!ind))**2.0]
                     : fsecNum (tail polec) (delete (zeroc!!ind) zeroc) zeror
    False-> let a1 = map (\i-> realPart (abs (head polec - i))) zeror
                mina1 = minimum a1
                ind1 = fromJust $ elemIndex mina1 a1
                tmpsecn1 = [1, -zeror!!ind1]
                zeror1 = delete (zeror!!ind1) zeror
                a2 = map (\i-> realPart (abs (head polec - i))) zeror1
                mina2 = minimum a2
                ind2 = fromJust $ elemIndex mina2 a2
                tmpsecn2 = map realPart $ convolve tmpsecn1 [1, -zeror1!!ind2]  -- ToDo Check it
                zeror2 = delete (zeror1!!ind2) zeror1
             in tmpsecn2 : fsecNum (tail polec) zeroc zeror2


convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate (length hs - 1) 0
      ts  = pad ++ xs
   in map (sum . zipWith (*) (reverse hs)) (init $ tails ts)












