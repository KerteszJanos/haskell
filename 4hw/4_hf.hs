import Data.Char

----------------------------------1
(+++) :: [a] -> [a] -> [a]
(+++) a b = concat[a,b]


----------------------------------2
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) i = (!!!) xs (i-1)

----------------------------------3
longerThan :: Integral i => [a] -> i -> Bool
longerThan [] _ = False
longerThan _ 0 = True
longerThan (x:xs) i = longerThan xs (i-1)


----------------------------------4
space :: Char -> String
space c
  | isUpper(c) = " "++[c]
  | otherwise = ""++[c]

camelToWords :: String -> String
camelToWords s = concat[space(x) | x <- s]


----------------------------------5
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (x:xs) = [x:xs]++(tails' xs)