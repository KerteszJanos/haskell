-----------------------------------------------------------------------------------------1.1
data Data a = Data a (Int,Int) | Empty (Int,Int) deriving (Show,Eq)

type DataBase a = [Data a]
-----------------------------------------------------------------------------------------1.2
getData :: (Int, Int) -> DataBase a -> Data a
getData (n,m) [] = Empty (n,m)
getData (n,m) (Data ertek tuple:xs)
  | tuple == (n,m) = (Data ertek tuple)
  | otherwise = getData (n,m) xs
getData (n,m) (Empty tuple:xs)
  | tuple == (n,m) = (Empty tuple)
  | otherwise = getData (n,m) xs

-----------------------------------------------------------------------------------------1.3
sumOfData :: DataBase Int -> Int
sumOfData [] = 0
sumOfData (Data ertek tuple:xs) = ertek + sumOfData xs
sumOfData (Empty tuple:xs) = sumOfData xs


-----------------------------------------------------------------------------------------2.1
data PersonTrait = Name | Age | Eye deriving (Eq, Show)


-----------------------------------------------------------------------------------------2.2
cleanedPerson :: [PersonTrait] -> [[(PersonTrait,String)]] -> [[(PersonTrait,Maybe String)]]
cleanedPerson _ [] = []
cleanedPerson traits (x:xs) = (traitshelper traits x):cleanedPerson traits xs

traitshelper :: [PersonTrait] -> [(PersonTrait,String)] -> [(PersonTrait,Maybe String)]
traitshelper [] _ = []
traitshelper (Name:xs) l
  | sum[1 | x <- l, fst x == Name] == 1 = (Name,Just (((\[x] -> x) [snd(x) | x <- l, fst x == Name]))):traitshelper xs l
  | otherwise = (Name,Nothing):traitshelper xs l
  
traitshelper (Age:xs) l
  | sum[1 | x <- l, fst x == Age] == 1 = (Age,Just (((\[x] -> x) [snd(x) | x <- l, fst x == Age]))):traitshelper xs l
  | otherwise = (Age,Nothing):traitshelper xs l
  
traitshelper (Eye:xs) l  
  | sum[1 | x <- l, fst x == Eye] == 1 = (Eye,Just (((\[x] -> x) [snd(x) | x <- l, fst x == Eye]))):traitshelper xs l
  | otherwise = (Eye,Nothing):traitshelper xs l
    
  
  
  
  
  
  