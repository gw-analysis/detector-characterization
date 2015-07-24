-- Time-stamp: "2015-07-25 02:48:06 yuzu"
module UsefulFunction (
       taple2string
       ,threedata2string
       ,string2Int
       ,string2Double
       ,skipListByK
       ,fst'
       ,snd'
       ,trd'
) where


-- taple2string [1, 2, 3, 4] [4, 5, 6, 7]
-- -> "1 4\n2 5\n3 6\n4 7\n"
-- you can easily write ANSII txt, such as 
--   writeFile "hoge.txt" $ taple2string [1, 2, 3, 4] [4, 5, 6, 7]
taple2string ::[Double] -> [Double] -> String
taple2string a b = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  (map show b)

threedata2string ::[Double] -> [Double] ->[Double]-> String
threedata2string a b c = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  (map show c)


-- string2Int "3"
-- -> return 3::Int
string2Int :: String -> Int
string2Int str = read str::Int


-- string2Int "3"
-- -> return 3.0::Double
string2Double :: String -> Double
string2Double str = read str::Double


-- skipListByK 2 0 [5.0, 2.0, 3.0, 5.0, 9.0]
--   return [2.0,5.0]
-- you can get skiped list by k.
-- Another case :
-- skipListByK 2 0 [5.0, 2.0, 3.0, 5.0, 9.0]
--   return [5.0,3.0,9.0]
-- you can get skiped list by k.

skipListByK :: Int -> Int -> [a] -> [a]
skipListByK k rest lst = map snd $ filter (\x -> (fst x) `mod` k == rest) $ zip [1..] lst



fst' :: (Double, Double, Double)  -> Double
fst' (a, b, c) = a

snd' :: (Double, Double, Double)  -> Double
snd' (a, b, c) = b

trd' :: (Double, Double, Double)  -> Double
trd' (a, b, c) = c
