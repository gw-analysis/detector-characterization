


module HasKAL.Misc.Flip3param (
   flip132
  ,flip213
  ,flip231
  ,flip312
  ,flip321
) where

flip132 :: (a -> b -> c -> d) -> a -> c -> b -> d
flip132 = (flip.)

flip213 :: (a -> b -> c -> d) -> b -> a -> c -> d
flip213 = flip

flip231 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip231 = (flip.).flip

flip312 :: (a -> b -> c -> d) -> c -> a -> b -> d
flip312 = flip.(flip.)

flip321 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip321 = flip.(flip.).flip

