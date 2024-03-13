import Data.List

-------------------------------------1
getSum :: Int -> Int -> Int
getSum a b = sum[x | x <- [a..b]++[b..a]]


-------------------------------------2
isPrime :: Int -> Bool
isPrime a = null[x | x <- [2..a `div` 2], a `mod` x == 0]

primeList :: Int -> Int -> [Int]
primeList 0 b = [x | x <- [2..b], isPrime(x)]
primeList 1 b = [x | x <- [2..b], isPrime(x)]
primeList a b = [x | x <- [a..b], isPrime(x)]


------------------------------------3
encode :: String -> [(Char,Int)]
encode str = [(head(x),length(x)) | x <- group(str)]


------------------------------------4
decode' :: [(Char,Int)] -> [String]
decode' a = [replicate (snd x) (fst x) | x <- a]

decode :: [(Char,Int)] -> String
decode a = concat (decode' a)