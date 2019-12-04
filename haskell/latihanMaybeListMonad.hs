{-
Saya mencoba latihan case of supaya lebih paham cara kerjanya untuk belajar Monad
Syntax case of adalah sebagai berikut:

case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
-}

import Data.List

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  

-- Jika [], keluar "empty". Jika [x], keluar "a singleton list". Jika xs, keluar "a longer list". Setelah itu diconcat dengan "The list is"

{-
Sekarang untuk case of dalam contoh Monad
Ingat ini ya
data Maybe a = Nothing
             | Just a
lookup :: a -> [(a, b)] -> Maybe b
-}

animalFriends :: [(String, String)]
animalFriends = [ ("Pony", "Lion")
                , ("Lion", "Manticore")
                , ("Manticore", "Unicorn")
                , ("Unicorn", "Lepricon") ]

-- Does Pony's friend have a friend in animalMap?
animalFriendLookup :: [(String, String)] -> Maybe String
animalFriendLookup animalMap =
  case lookup "Pony" animalMap of
-- Jika lookup "Pony" animalMap mengeluarkan Nothing
       Nothing -> Nothing
-- Jika lookup "Pony" animalMap mengeluarkan Just ponyfriend	
       Just ponyFriend ->
         case lookup ponyFriend animalMap of
-- Jika lookup ponyFriend animalMap mengeluarkan Nothing
              Nothing -> Nothing
-- Jika lookup ponyFriend animalMap mengeluarkan Just ponyFriendFriend
              Just ponyFriendFriend ->  
                case lookup ponyFriendFriend animalMap of
-- Jika lookup ponyFriendFriend mengeluarkan Nothing
                  Nothing -> Nothing
-- Jika lookup ponyFriendFriend mengeluarkan Just ponyFriendFriendFriend
                  Just ponyFriendFriendFriend ->
                    case lookup ponyFriendFriendFriend animalMap of
-- Jika ponyFriendFriendFriend mengeluarkan Nothing
                        Nothing -> Nothing
-- Jika ponyFriendFriendFriend punya friend
                        Just friend -> Just friend

{-
Outputnya adalah Just "Lepricorn".
"Pony" punya teman "Lion" (ponyFriend). 
"Lion" punya teman "Manticore" (ponyFriendFriend). 
"Manticore" punya teman "Unicorn" (ponyFriendFriendFriend). 
"Unicorn" punya teman "Lepricorn" (friend).
-}

{-
Monad didefinisikan dengan tipe konstruktor m, fungsi return, dan operator bind (>>=)
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b

Misalnya untuk Maybe monad, definisinya adalah sebagai berikut
    return :: a -> Maybe a
    return x  = Just x

    (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
    m >>= g = case m of
                 Nothing -> Nothing
                 Just x  -> g x

Cara memanfaatkan monad dalam masalah yang sama di latihanCaseOf, yang mencari teman dari teman "Pony".
Dengan menggunakan monad, pertama lakukan lookup "Pony" animalMap. Jika terdapat Just ponyFriend (bukan Nothing), lakukan lookup ponyFriend animalMap. Jika terdapat pony2ndFriend (bukan Nothing), lakukan lookup
pony2ndFriend animalMap. Jika terdapat friend (bukan Nothing), kembalikan Just friend.
Outputnya adalah Unicorn yang merupakan teman temannya teman Pony 0_0
-}
monadicFriendLookup :: [(String, String)] -> Maybe String
monadicFriendLookup animalMap =
  lookup "Pony" animalMap
  >>= (\ponyFriend -> lookup ponyFriend animalMap
  >>= (\pony2ndFriend -> lookup pony2ndFriend animalMap
  >>= (\friend -> Just friend)))

-- y >>= \x -> f x ekivalen dengan x <- y

sugaryFriendLookup :: [(String, String)] -> Maybe String
sugaryFriendLookup animalMap = do
  ponyFriend    <- lookup "Pony" animalMap
  ponyFriend'   <- lookup ponyFriend animalMap
  ponyFriend''  <- lookup ponyFriend' animalMap
  return ponyFriend''

{-
test0 menggunakan list comprehension untuk menyatukan list x dan y menjadi tuple
test1 menggunakan do
test2 menggunakan monad
-}
test0 = [(x,y)| x<-[1,2,3],y<-[4,5]]
test1 = do
        x <- [1,2,3]
        y <- [4,5]
        return (x,y)
test2 = [1,2,3] >>= \x -> [4,5] >>= \y -> return (x,y)
{-
List itu ternyata monad
Ingat (>>=)  :: m a -> (a -> m b) -> m b
Jika terdapat x dalam [1,2,3], lakukan monad kedua yaitu jika terdapat y dalam [4,5] lakukan return (x,y) yang mengembalikan [(x,y)]
Output yang diharapkan adalah [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

Seperti yang dijelaskan di ContohMonad dalam latihan kelas:

instance  Monad []  where	    
    m >>= k	=  concat (map k m)	    
    return x	=  [x]	    
    fail x	=  [ ]

tahap 1
test_1 = concat (map (\x -> [4,5] >>= (\y -> return (x,y))) 
                    [1,2,3])

tahap 2
[4,5] >>= (\y -> return (x,y)) adalah concat (map (\y -> return (x,y) [4,5] )
test_2 = concat (map (\x -> concat (map (\y -> return (x,y)) [4,5]))
                     [1,2,3])

tahap 3
return (x,y) = [(x,y)], seperti deklarasi return di penjelasan awal
test_3 = concat (map (\x -> concat (map (\y -> [(x,y)]) [4,5] )  ) 
                     [1,2,3])

tahap 4
test4 = concat (map (\x -> (concat (map (\y -> [(x,y)])
                                        [4,5])))
                    [1,2,3])
Output yang diharapkan adalah [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
-}