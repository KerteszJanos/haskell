module Homework10 where
import Data.List

----------------------------------------------------------------------------------------1
isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

----------------------------------------------------------------------------------------2
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust  Nothing = error "F"

----------------------------------------------------------------------------------------3
catMaybes :: [Maybe a] -> [a]
catMaybes l = [a | (Just a) <- l]

----------------------------------------------------------------------------------------4
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f l = [a | (Just a) <- (map f l)]

----------------------------------------------------------------------------------------5
type Username = String
type Password = String

data Privilege = Simple | Admin
  deriving (Eq, Show)
  
data Cookie = LoggedOut | LoggedIn Username Privilege
  deriving (Eq, Show)
  
data Entry = Entry Password Privilege [Username]
  deriving (Eq, Show)

type Database = [(Username, Entry)]

--------------------------------------------------------------------------5.1
----------------------------------------5.1.1
password :: Entry -> Password
password (Entry a b c) = a
----------------------------------------5.1.2
privilege :: Entry -> Privilege
privilege (Entry a b c) = b
----------------------------------------5.1.3
friends :: Entry -> [Username]
friends (Entry a b c) = c

--------------------------------------------------------------------------5.2
mkCookie :: Username -> Password -> Entry -> Cookie
mkCookie un pw (Entry a b c)
  | pw == password (Entry a b c) = LoggedIn un (privilege (Entry a b c))
  | otherwise = LoggedOut
  
--------------------------------------------------------------------------5.3
login :: Username -> Password -> Database -> Cookie
login _ _ [] = LoggedOut
login us pw (x:xs)
  | (loginhelper us pw x) = (mkCookie us pw (snd x))
  | otherwise = login us pw xs

loginhelper :: Username -> Password -> (Username, Entry) -> Bool
loginhelper us pw (a,b)
  | us == a && pw == (password b) = True
  | otherwise = False

--------------------------------------------------------------------------5.4
updateEntry :: Username -> (Username, Entry) -> Maybe (Username, Entry)
updateEntry un (tun,(Entry a b c))
  | un == tun = Nothing
  | otherwise = Just (tun, (Entry a b (filter (un/=) c)))

--------------------------------------------------------------------------5.5
deleteUser :: Cookie -> Username -> Database -> Database
deleteUser LoggedOut _ a = a
deleteUser (LoggedIn a b) un l
  | b /= Admin || (filter (\(a,b) -> a == un) l) == [] = l
  | otherwise = mapMaybe (updateEntry un) l 




richard, charlie, carol, david, kate :: (Username, Entry)
richard = ("Richard", Entry "password1" Admin  ["Kate"])
charlie = ("Charlie", Entry "password2" Simple ["Carol"])
carol   = ("Carol",   Entry "password3" Simple ["David", "Charlie"])
david   = ("David",   Entry "password4" Simple ["Carol"])
kate    = ("Kate",    Entry "password5" Simple ["Richard"])

testDB :: Database
testDB = [ richard, charlie, carol, david, kate ]

testDBWithoutCarol :: Database
testDBWithoutCarol =
  [ ("Richard", Entry "password1" Admin  ["Kate"])
  , ("Charlie", Entry "password2" Simple [])
  , ("David",   Entry "password4" Simple [])
  , ("Kate",    Entry "password5" Simple ["Richard"])
  ]