



module Function
( complement
, dilationBinary
, erosionBinary
, dilationGrey
, erosionGrey
) where



import Data.List (maximum, minimum)
import Data.Set (empty, member, insert, intersection,isSubsetOf, fromList, toList)



complement :: (Eq a, Show a, Num a, Ord a)=> [(Int, Int, a)] -> [(Int, Int, a)]
complement x = let maxx = maximum [y|(_, _, y)<-x]
                   minx = minimum [y|(_, _, y)<-x]
                in [(b, c, maxx+minx-d)|(b, c, d)<-x]


dilationBinary :: [(Int,Int)] ->  [(Int,Int)] -> [(Int,Int)]
dilationBinary b x = nub' [(b1+x1,b2+x2)|(b1,b2)<-b,(x1,x2)<-x]


erosionBinary :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
erosionBinary b x =  filter (\y-> isSubsetOf (fromList [(x1+b1,x2+b2)|(b1,b2)<-b,let x1 = fst y, let x2 = snd y]) (fromList x)) x


erosionGrey :: (Show a, Eq a, Num a, Ord a) => [((Int,Int),a)] -> [((Int,Int),a)] -> [((Int,Int),a)]
erosionGrey s x = let z = unzip $ intersected s x
                      valz = snd z
                      ind = fst z
                   in zip ind $ replicate (length valz) (minimum valz)


dilationGrey:: (Show a, Eq a, Num a, Ord a) => [((Int,Int),a)] -> [((Int,Int),a)] -> [((Int,Int),a)]
dilationGrey s x = let (ind, valz) =  unzip $ intersected s x
                    in zip ind $ replicate (length valz) (maximum valz)


intersected :: (Show a, Eq a, Num a, Ord a) => [((Int,Int),a)] -> [((Int,Int),a)] -> [((Int,Int),a)]
intersected s x = filter (\a->isIntersect a s x) x
 where
  isIntersect a s x =
   not . null . toList $
    intersection (fromList [(a1+s1,a2+s2)|((s1,s2),_)<-s,let a1=fst (fst a),let a2=snd (fst a)])
                 (fromList [(x1, x2)|((x1, x2), _)<-x])



nub' :: (Ord a) => [a] -> [a]
nub' l = nub'' l empty
    where nub'' [] _         = []
          nub'' (x:xs) s
              | x `member` s = nub'' xs s
              | otherwise    = x : nub'' xs (x `insert` s)

