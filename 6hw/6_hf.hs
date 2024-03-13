import Data.List

filters :: (a -> Bool) -> [[a]] -> [a]
filters f ll = filter f (concat ll)
--filters f ll = filter f (map ll)
--------------------------------------------------------------
mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap f ll = map (map f) ll
--------------------------------------------------------------
dropSpaces :: [Char] -> [Char]
dropSpaces t = dropWhile (==' ') t
--------------------------------------------------------------
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq [e] = [e]
uniq (x:xs) = x:uniq(filter (/=x) xs)