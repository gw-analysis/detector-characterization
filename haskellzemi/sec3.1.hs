lucky :: Int -> String
lucky 7 = "Lucky number 7"
lucky x = "sorry"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*factorial(n - 1)

head' :: [a] -> a
head' [] = error "Can't call"
head' (x:_) = x

firstl :: String -> String
firstl "" = "Empty"
firstl all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= 18.5 = "You are underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "fat"
  | otherwise = "whale"
  where bmi = weight/height^2

cylinder :: Double -> Double -> Double
cylinder r h = 
  let sideArea = 2*pi*r*h
      topArea = pi*r^2
  in sideArea + 2*topArea

calcBmis :: [(Double,Double)] -> [Double]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w/h^2]

head'' :: [a] -> a
head'' xs = case xs of [] -> error "empty"
                       (x:_) -> x


