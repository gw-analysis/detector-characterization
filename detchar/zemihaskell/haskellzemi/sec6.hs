import Data.List
import Data.Char
import qualified Data.Map as Map

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Int -> Maybe Int
firstTo40 n = find (\x -> digitSum x == n) [1..]

phoneBook =
  [("betty","333-5135")
  ,("bonnie", "325-5135")
  ,("daniel", "325-4155")
  ,("wenny", "351-5546")
  ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k,v):xs)
    | key == k = Just v
    | otherwise = findKey' key xs

phoneBook2 :: Map.Map String String
phoneBook2 =Map.fromList $
  [("betty","333-5135")
  ,("bonnie", "325-5135")
  ,("daniel", "325-4155")
  ,("wenny", "351-5546")
  ]

string2digits :: String -> [Int]
string2digits  = map digitToInt.filter isDigit

