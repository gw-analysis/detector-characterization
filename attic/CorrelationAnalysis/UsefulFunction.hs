-- Time-stamp: "2014-09-08 07:29:28 yuzurihara"
module UsefulFunction (
       taple2string
       ,string2Int
       ,string2Double
       ,skipListByK
) where


taple2string ::[Double] -> [Double] -> String
taple2string a b = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  (map show b)

string2Int :: String -> Int
string2Int str = read str::Int

string2Double :: String -> Double
string2Double str = read str::Double

skipListByK :: Int -> Int -> [a] -> [a]
skipListByK k rest lst = map snd $ filter (\x -> (fst x) `mod` k == rest) $ zip [1..] lst
