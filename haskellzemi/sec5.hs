multithree :: Int -> Int -> Int -> Int
multithree x y z = x*y*z

multTwoWithNice :: Int -> Int -> Int
multTwoWithNice = multithree 9

compareWithHunderd :: Int -> Ordering
compareWithHunderd x = compare 100 x

compareWithHunderd' :: Int -> Ordering
compareWithHunderd' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x
