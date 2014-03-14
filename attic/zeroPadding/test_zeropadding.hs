{-
- test code for zero padding format
-
-}

module Main where

main :: IO()
main = do
  let num = "5"
  print $ replicate (8-length num) '0' ++ num


