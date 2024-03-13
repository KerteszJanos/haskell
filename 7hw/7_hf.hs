-------------------------------------------------1
mapping :: [(Char,Char)]
mapping = zip [x | x <- ['0'..'9']++['A'..'Z']++['a'..'z']] [x | x <- ['3'..'9']++['A'..'Z']++['a'..'z']++['1']++['2']++['3']]

-------------------------------------------------2
encodeCaesar :: [Char] -> [Char]
encodeCaesar [] = [] 
encodeCaesar (y:ys) = [snd(x) | x <- mapping, fst(x) == y]++encodeCaesar ys

------------------------------------------------3
decodeCaesar :: [Char] -> [Char]
decodeCaesar [] = []
decodeCaesar (y:ys) = [fst(x) | x <- mapping, snd(x) == y]++decodeCaesar ys