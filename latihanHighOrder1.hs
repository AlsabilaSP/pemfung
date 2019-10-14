-- 1 (*). Exercises from chapter 9-10 of The Craft of Functional Programming

-- NO 1
-- Membuat fungsi length menggunakan map dan sum
panjang :: [a] -> Int
panjang xs = sum (map (\x -> 1) xs)

-- Contoh kalau tidak menggunakan map dan sum
puanjang :: [a] -> Int
puanjang [] = 0
puanjang (x:xs) = 1 + puanjang xs

-- NO 2
-- Apa yang dilakukan map (+1) (map (+1) xs)?
no2 xs = map (+1) (map (+1) xs)
-- Fungsi ini menjumlahkan setiap elemen dalam list dengan 2, mirip seperti bentuk fungsi komposisi f(g(x)) atau (f o g)(x) dimana f(x) = x+1 dan g(x) = x+1.
-- PS. Saya penasaran cara mendeklarasikannya, belum bisa :')

-- NO 3
-- iter 3 f x = f (f (f x))
-- Misal iter 3 (2*) 1 akan mengembalikan hasil dari 2*(2*(2*1))) yaitu 8
iter :: Int -> (a -> a) -> (a -> a)
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

-- NO 5
-- (*) How would you define the sum of the squares of the natural numbers 1 to n using map and foldr?
sumSquares n = foldr (+) 0 $ map (\x -> x*x) [1..n]
-- Misal n = 3, jadinya (3*3 + ( 2*2 + ( 1*1 + 0)))
-- Bisa juga ditulis (foldr (+) 0) (map (\x -> x*x) [1..n])
-- Tanda $ digunakan untuk menggantikan parentheses, yang kurung buka tutup itu supaya Haskell tidak salah baca argumennya 

-- alternative dengan lazy evaluation
lazySumSquares n = foldr (+) 0 [ x*x | x <- [1..n] ]
-- alternative dengan foldl
leftSumSquares n = foldl (+) 0 $ map (\x -> x*x) [1..n]
-- Misal n = 3, jadinya (((0 + 1*1) + 2*2) + 3*3)

-- NO 6
mystery xs = foldr (++) [] (map sing xs)
 where
  sing x = [x]
-- Fungsi ini memasukkan x yang ada di dalam xs ke dalam list lalu dikonkat dengan list kosong, jadi sebenarnya output yang dikeluarkan adalah xs sendiri

-- NO 7
{-
(id . f)  sama saja dengan f karena hasil dari f yang bertipe Boolean akan digunakan oleh identity function yang mengembalikan Boolean tersebut.
(id :: Bool -> Bool)

(f . id)  sama saja dengan f karena argumen dari f yaitu Integer digunakan oleh identity function yang mengembalikan Integer tersebut, lalu dijadikan argumen f.
(id :: Int -> Int)

id f      sama saja dengan f karena identity function tidak merubah output f.
(id :: (Int -> Bool) -> (Int -> Bool))
-}

-- NO 8
composeList :: [a -> a] -> (a -> a)
composeList []     = id
composeList (f:fs) = f . composeList fs
-- Fungsi ini mengambil list berisi fungsi-fungsi dengan tipe a -> a lalu mengembalikan satu fungsi yang tipenya a -> a juga.
-- Saya mengerti perilaku dari fungsi ini, tapi belum tahu input yang diterima seperti apa :')

-- NO 9
flippin :: (a -> b -> c) -> (b -> a -> c)
flippin f = \x y -> f y x
-- Fungsi flippin menerima suatu fungsi f sebagai input, nanti variabel di dalam fungsi tersebut akan ditukar
-- Output dari flippin adalah hasil dari fungsi tersebut yang sudah ditukar variabelnya.
-- flip div 3 100 menukar variabelnya jadi yang dijalankan adalah div 100 3 