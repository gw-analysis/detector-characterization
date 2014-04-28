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

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

squareroot :: Integer
squareroot = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n*3 + 1)

-- numLongChains :: Int
-- numLongChains = length (filter isLong (map chain [1..100]))
--   where isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a
sum' xs = foldl(\acc x -> acc + x) 0 xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x:acc) [] xs

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length $ takeWhile( < 100000)(scanl1( + )(map sqrt [1..]))

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile ( < 10000) . filter odd $ map (^2) [1..]
