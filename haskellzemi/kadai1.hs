factorialguard :: Int -> Int
factorialguard n
  | n < 0 = error "why did you insert the Minos number?"
  | n == 0 = 1
  | otherwise = n * factorialguard (n - 1)

factorialif :: Int -> Int
factorialif n = if n < 0
                then error "why did you insert the Minos number?"
                else if n == 0
                     then 1
                     else n*factorialif (n - 1)

factorialcase :: Int -> Int
factorialcase n = case n of 0 -> 1
                            otherwise ->  n*factorialcase (n - 1)
-- すごく負けた気がする...

uruucal :: Int -> String
uruucal xs
  | mod xs 400 == 0 = "uruu!"
  | mod xs 100 == 0 = "non uruu..."
  | mod xs 4 == 0 = "uruu!"
  | otherwise = "non uruu..."

