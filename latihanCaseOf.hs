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
"Pony" punya teman "Lion". "Lion" punya teman "Manticore". "Manticore" punya teman "Unicorn". "Unicorn"
punya teman "Lepricorn". Jadi outputnya "Lepricorn" mirip seperti fungsi lookup.
-}