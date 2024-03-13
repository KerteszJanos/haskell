double :: Int -> Int
double a = a*2

greaterThenTwo :: Int -> Bool
greaterThenTwo a = a `mod` 5 > 2

volume :: Int -> Int -> Int -> Int
volume a b c = a*b*c