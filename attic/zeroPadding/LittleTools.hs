module LittileTools
(
num2zpnum
) where

num2zpnum :: Int -> Int -> String
num2zpnum totalnum num = replicate (totalnum - length strnum) '0' ++ strnum
  where
    strnum = show num
