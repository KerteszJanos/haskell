import Data.List
-------------------------------------------------------------------------------------------1
pairsWithPred :: (a -> a -> Bool) -> [a] -> [(a,a)]
pairsWithPred p l = filter (\(x,y) -> p x y) [(x,y) | x <- l, y <- l]


-------------------------------------------------------------------------------------------2
appLeftOrRight :: [a -> b] -> [a -> b] -> (a -> Bool) -> [a] -> [b]
appLeftOrRight _ _ _ [] = []
appLeftOrRight _ [] _ _ = []
appLeftOrRight [] _ _ _ = []
appLeftOrRight (f:fs) (g:gs) p (x:xs)
  | p x = (f x):appLeftOrRight fs gs p xs
  |otherwise = (g x):appLeftOrRight fs gs p xs


-------------------------------------------------------------------------------------------3
deletions :: [a] -> [[a]]
deletions l = map (\(a,b) -> a++b) (zip (init $ inits l) (tail $ tails l))
-------------------------------------------------------------------------------------------4
secondParam :: Integral a => [a -> a -> Bool] -> (a,a) -> a -> [a]
secondParam fl (e,u) a = [x | x <- [e..u], (length fl) == (length [y | y <- fl, y a x])]


-------------------------------------------------------------------------------------------5
fixPoint :: Eq a => (a -> a) -> a -> a
fixPoint f a
  | f a == a = a
  | otherwise = fixPoint f (f a)