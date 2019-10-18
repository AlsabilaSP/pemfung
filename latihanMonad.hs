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
-}

-- Cara memanfaatkan monad dalam masalah yang sama di latihanCaseOf, yang mencari teman dari teman "Pony".
animalFriends :: [(String, String)]
animalFriends = [ ("Pony", "Lion")
                , ("Lion", "Manticore")
                , ("Manticore", "Unicorn")
                , ("Unicorn", "Lepricon") ]

{-
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
Jika terdapat x dalam [1,2,3], lakukan monad kedua yaitu jika terdapat y dalam [4,5] lakukan return (x,y)
Output yang diharapkan adalah [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
-}