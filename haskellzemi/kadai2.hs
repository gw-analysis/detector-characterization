-- 課題2.1:フィボナッチ数列の実装
fibonacciNum :: Int -> Int
fibonacciNum n
  | n < 0 = error "Why did you insert a minus number?"
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibonacciNum(n - 1) + fibonacciNum(n - 2)

-- 課題2.2:ユークリッドの互除法の実装
euclideanAlg :: Int -> Int -> Int
euclideanAlg x y
  | x < 0 = error "Why did you insert a minus number?"
  | y < 0 = error "Why did you insert a minus number?"
  | rem x y == 0 = y
  | otherwise = euclideanAlg y (rem x y) 

-- 課題2.3:配列を平らにする
listSmoother :: [[a]] -> [a]
listSmoother xs
  | null (tail xs) == True = (head xs)
  | otherwise = listSmoother ([((head xs) ++ head (tail xs))] ++ tail (tail xs))
