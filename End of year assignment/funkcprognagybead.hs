import Data.Maybe
import Data.List

type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int
    deriving (Eq,Show)
data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int
    deriving (Eq,Show)

data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)]
    deriving (Eq,Show)


tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel s lp lz) (n,m) (Peashooter h)
    | s >= 100 &&
      n <= 4 && n >=0 && m <= 11 && m >=0 &&
      lookup (n,m) lp == Nothing = Just (GameModel (s-100) (((n,m),(Peashooter h)):lp) lz)
    | otherwise = Nothing
tryPurchase (GameModel s lp lz) (n,m) (CherryBomb h)
    | s >= 150 &&
      n <= 4 && n >=0 && m <= 11 && m >=0 &&
      lookup (n,m) lp == Nothing = Just (GameModel (s-150) (((n,m),(CherryBomb h)):lp) lz)
    | otherwise = Nothing
tryPurchase (GameModel s lp lz) (n,m) (Sunflower h)
    | s >= 50 &&
      n <= 4 && n >=0 && m <= 11 && m >=0 &&
      lookup (n,m) lp == Nothing = Just (GameModel (s-50) (((n,m),(Sunflower h)):lp) lz)
    | otherwise = Nothing
tryPurchase (GameModel s lp lz) (n,m) (Walnut h)
    | s >= 50 &&
      n <= 4 && n >=0 && m <= 11 && m >=0 &&
      lookup (n,m) lp == Nothing = Just (GameModel (s-50) (((n,m),(Walnut h)):lp) lz)
    | otherwise = Nothing


placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel s lp lz) z n
    | n <= 4 && n >=0 &&
      lookup (n,11) lz == Nothing = Just (GameModel s lp (((n,11),z):lz))
    | otherwise = Nothing



performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel s lp lz)
    | any (\p -> lookup p lz /= Nothing) [(x,y) | x <- [0..4], y <- [-1,0]] = Nothing
    | otherwise = Just (GameModel s (pSeged lp lz) (zSeged lp lz))

zSeged :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
zSeged lp [] = []
zSeged lp ( ((n,m),(Basic h speed)):xs )
    | lookup (n,m) lp == Nothing = ((n,m-speed),(Basic h speed)):zSeged lp xs
    | otherwise = ((n,m),(Basic h speed)):zSeged lp xs
zSeged lp ( ((n,m),(Conehead h speed)):xs )
    | lookup (n,m) lp == Nothing = ((n,m-speed),(Conehead h speed)):zSeged lp xs
    | otherwise = ((n,m),(Conehead h speed)):zSeged lp xs
zSeged lp ( ((n,m),(Buckethead h speed)):xs )
    | lookup (n,m) lp == Nothing = ((n,m-speed),(Buckethead h speed)):zSeged lp xs
    | otherwise = ((n,m),(Buckethead h speed)):zSeged lp xs
zSeged lp ( ((n,m),(Vaulting h speed)):xs )
    | lookup (n,m) lp == Nothing && lookup (n,m-1) lp == Nothing = ((n,m-speed),(Vaulting h speed)):zSeged lp xs
    | lookup (n,m) lp /= Nothing = ((n,m-1),(Vaulting h (speed-1))):zSeged lp xs 
    | lookup (n,m-1) lp /= Nothing = ((n,m-speed),(Vaulting h (speed-1))):zSeged lp xs
    | otherwise = ((n,m),(Vaulting h (speed-1))):zSeged lp xs

pSeged :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Plant)]
pSeged [] lz = []
pSeged (((n,m),(Peashooter h)):xs) lz
    | lookup (n,m) lz /=Nothing = ((n,m),(Peashooter (h-(pSegedseged (n,m) lz)))):pSeged xs lz
    | otherwise = ((n,m),(Peashooter h)):pSeged xs lz
pSeged (((n,m),(Sunflower h)):xs) lz
    | lookup (n,m) lz /=Nothing = ((n,m),(Sunflower (h-(pSegedseged (n,m) lz)))):pSeged xs lz
    | otherwise = ((n,m),(Sunflower h)):pSeged xs lz
pSeged (((n,m),(Walnut h)):xs) lz
    | lookup (n,m) lz /=Nothing = ((n,m),(Walnut (h-(pSegedseged (n,m) lz)))):pSeged xs lz
    | otherwise = ((n,m),(Walnut h)):pSeged xs lz
pSeged (((n,m),(CherryBomb h)):xs) lz
    | lookup (n,m) lz /=Nothing = ((n,m),(CherryBomb (h-(pSegedseged (n,m) lz)))):pSeged xs lz
    | otherwise = ((n,m),(CherryBomb h)):pSeged xs lz

pSegedseged :: (Int,Int) -> [(Coordinate, Zombie)] -> Int
pSegedseged _ [] = 0
pSegedseged t (((n,m),(Basic h speed)):xs)
    | t == (n,m) = 1+pSegedseged t xs
    | otherwise = pSegedseged t xs
pSegedseged t (((n,m),(Conehead h speed)):xs)
    | t == (n,m) = 1+pSegedseged t xs
    | otherwise = pSegedseged t xs
pSegedseged t (((n,m),(Buckethead h speed)):xs)
    | t == (n,m) = 1+pSegedseged t xs
    | otherwise = pSegedseged t xs
pSegedseged t (((n,m),(Vaulting h speed)):xs)
    | t == (n,m) && speed == 1 = 1+pSegedseged t xs
    | otherwise = pSegedseged t xs





cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel s lp lz) = (GameModel s (pTorol lp) (zTorol lz))

zTorol :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
zTorol [] = []
zTorol (((n,m),(Basic h speed)):xs)
    | h <= 0 = zTorol xs
    | otherwise = ((n,m),(Basic h speed)):zTorol xs
zTorol (((n,m),(Conehead h speed)):xs)
    | h <= 0 = zTorol xs
    | otherwise = ((n,m),(Conehead h speed)):zTorol xs
zTorol (((n,m),(Buckethead h speed)):xs)
    | h <= 0 = zTorol xs
    | otherwise = ((n,m),(Buckethead h speed)):zTorol xs
zTorol (((n,m),(Vaulting h speed)):xs)
    | h <= 0 = zTorol xs
    | otherwise = ((n,m),(Vaulting h speed)):zTorol xs

pTorol :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
pTorol [] = []
pTorol (((n,m),(Peashooter h)):xs)
    | h <= 0 = pTorol xs
    | otherwise = ((n,m),(Peashooter h)):pTorol xs
pTorol (((n,m),(Sunflower h)):xs)
    | h <= 0 = pTorol xs
    | otherwise = ((n,m),(Sunflower h)):pTorol xs
pTorol (((n,m),(Walnut h)):xs)
    | h <= 0 = pTorol xs
    | otherwise = ((n,m),(Walnut h)):pTorol xs
pTorol (((n,m),(CherryBomb h)):xs)
    | h <= 0 = pTorol xs
    | otherwise = ((n,m),(CherryBomb h)):pTorol xs





defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2