



module Function
( complement
, dilationBinary
, erosionBinary
, dilationGrey
, erosionGrey
) where



import Data.List (maximum, minimum, lookup)
import Data.Maybe (fromJust)
import Data.Set (empty, member, insert, intersection,isSubsetOf, fromList, toList)



complement :: (Eq a, Show a, Num a, Ord a)=> [(Int, Int, a)] -> [(Int, Int, a)]
complement x = let maxx = maximum [y|(_, _, y)<-x]
                   minx = minimum [y|(_, _, y)<-x]
                in [(b, c, maxx+minx-d)|(b, c, d)<-x]


dilationBinary :: [(Int,Int)] -> [(Int, Int)] ->  [(Int,Int)] -> [(Int,Int)]
dilationBinary z b x = toList $ intersection (fromList (nub' [(b1+x1,b2+x2)|(b1,b2)<-b,(x1,x2)<-x]))
                                             (fromList z)


erosionBinary :: [(Int,Int)] -> [(Int, Int)] -> [(Int,Int)] -> [(Int,Int)]
erosionBinary z b x = toList $ intersection
  (fromList (filter (\y-> isIn b x y) x))
  (fromList z)
  where isIn b x y = isSubsetOf (fromList [ (y1+b1,y2+b2)
                                          | (b1,b2)<-b
                                          , let y1 = fst y
                                          , let y2 = snd y
                                          , y1+b1>=b1L
                                          , y1+b1<=b1U
                                          , y2+b2>=b2L
                                          , y2+b2<=b2U]) (fromList x)
        b1L = minimum . fst . unzip $ z
        b1U = maximum . fst . unzip $ z
        b2L = minimum . snd . unzip $ z
        b2U = maximum . snd . unzip $ z


erosionGrey :: (Show a, Eq a, Num a, Ord a) => [((Int,Int),a)] -> [((Int,Int),a)] -> [((Int,Int),a)]
erosionGrey s x = (flip map) x $ \a->updateIntersection minimum s x a


dilationGrey:: (Show a, Eq a, Num a, Ord a) => [((Int,Int),a)] -> [((Int,Int),a)] -> [((Int,Int),a)]
dilationGrey s x = (flip map) x $ \a->updateIntersection maximum s x a


updateIntersection :: (Show a, Eq a, Num a, Ord a)
                   => ([a] -> a)
                   -> [((Int,Int),a)]
                   -> [((Int,Int),a)]
                   -> ((Int,Int),a)
                   -> ((Int,Int),a)
updateIntersection f s x a = let axs = [ (a1+s1,a2+s2)
                                       | ((s1,s2),1)<-s
                                       , let a1 = fst . fst $ a
                                       , let a2 = snd . fst $ a
                                       , a1+s1>=x1L
                                       , a1+s1<=x1U
                                       , a2+s2>=x2L
                                       , a2+s2<=x2U]
                                 faxs = f $ (flip map) axs $ \y-> fromJust (lookup y x)
                              in ((fst . fst $ a, snd . fst $ a), faxs)

 where
  x1L = minimum . fst . unzip . fst . unzip $ x
  x1U = maximum . fst . unzip . fst . unzip $ x
  x2L = minimum . snd . unzip . fst . unzip $ x
  x2U = maximum . snd . unzip . fst . unzip $ x



nub' :: (Ord a) => [a] -> [a]
nub' l = nub'' l empty
    where nub'' [] _         = []
          nub'' (x:xs) s
              | x `member` s = nub'' xs s
              | otherwise    = x : nub'' xs (x `insert` s)

