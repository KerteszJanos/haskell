origo :: (Int,Int) -> Bool
origo (a, b) = a==0 && b==0

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

isSmallPrime :: Int -> Bool
isSmallPrime a = a==2 || a==3 || a==5 || a==7

invertO :: (Int,Int) -> (Int,Int)
invertO (a,b) = (a*(-1),b*(-1))

divides :: Int -> Int -> Bool
divides 0 0 = True
divides 0 _ = False
divides a b = b `mod` a  == 0

add :: (Int,Int) -> (Int,Int) -> (Int,Int)
add (a1,a2) (b1, b2) = (a1+b1,a2+b2)

sub :: (Int,Int) -> (Int,Int) -> (Int,Int)
sub (a1,a2) (b1, b2) = (a1-b1,a2-b2)

scalar :: (Int,Int) -> (Int,Int) -> Int
scalar (a1,a2) (b1, b2) = a1*b1+a2*b2