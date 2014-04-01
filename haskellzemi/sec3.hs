lucky ::  Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you are out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "For"
sayMe x = "out of order"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*factorial (n - 1)

-- addVec :: (Double, Double) -> (Double, Double) -> (Double, Double)
-- addVec (x1,y1) (x2,y2) -> (x1 + x2,y1 + y2)

bimTell :: Double -> Double -> String
bimTell weight height
  | weight/height^2 <= 18.5 = "te"
  | otherwise =  "wakaran"
