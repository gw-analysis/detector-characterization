module Polynomial
( polyeval

) where

polyeval :: Num a=> [a] -> a -> a
polyeval [] _ = 0
polyeval (p:ps) x = p + x*polyeval ps x

