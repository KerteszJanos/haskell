import Data.List
------------------------------------------------------------------------------------------1
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [e] = []
pairs (x:y:xs) = (x,y):(pairs xs)


------------------------------------------------------------------------------------------2
countString :: String -> String -> Int
countString _ [] = 0
countString l (x:xs)
  | not(isPrefixOf l (x:xs)) = 0 + (countString l xs)
  | otherwise = 1 + (countString l xs)
  

------------------------------------------------------------------------------------------3
sameParity :: [Int] -> Bool
sameParity [] = True
sameParity l = null[x | x <-l,even(x)] || length(l) == length[x | x <-l,even(x)]


------------------------------------------------------------------------------------------4
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [e] = True
isSorted (x:y:xs)
  | x > y = False
  | otherwise = isSorted (y:xs)
  
  
------------------------------------------------------------------------------------------5
uniques :: Eq a => [a] -> [a]
uniques l = [head(x) | x <- group(l)]