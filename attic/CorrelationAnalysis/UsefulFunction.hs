-- Time-stamp: "2014-09-08 06:43:04 yuzurihara"
module UsefulFunction.hs (
       taple2string
       ,string2Int
       ,string2Double
) where



taple2string ::[Double] -> [Double] -> String
taple2string a b = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  (map show b)

string2Int :: String -> Int
string2Int str = read str::Int

string2Double :: String -> Double
string2Double str = read str::Double
