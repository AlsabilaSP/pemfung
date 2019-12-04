-- Lazy Evaluation

import Data.List

-- NO 1
{-
1. Ekspresi mengambil input list x = [1,2,3,4] dan list y = [2,3,4]
2. Melakukan penjumlahan tiap elemen x+y untuk x > y, berarti pasangan (3,2),(4,2),(4,3)
3. Hasilnya dimasukkan ke dalam list output 
-}

-- NO 2
divisor n = [ x | x <- [1..n], n `mod` x == 0 ]

-- NO 3
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] 
                   ++ [x] ++ 
                   quickSort [y | y <- xs, y > x]

-- NO 4
-- x = [2,3,4] \\ [3] artinya mengeluarkan 3 dari list x, jadi output x = [2,4]
perm [] = [[]]
perm ls = [ x:ps | x <- ls, ps <- perm(ls\\[x])]

-- NO 5
primes = sieve [2 .. 10]
  where sieve (x:xs) = x : (sieve [z | z <- xs, z `mod` x /= 0])
{-
Saringan Erastosthenes
1. Tulis semua bilangan, mulai dari 1 sampai n. Misalkan ini adalah daftar A.
2. Buat suatu daftar yang masih kosong, sebut saja daftar B.
3. Coret bilangan 1 dari daftar A.
4. Lalu tulis 2 pada daftar B. Lalu coret 2 dan semua kelipatannya dari daftar A
5. Bilangan pertama yang belum tercoret dari daftar A (misalnya 3) adalah bilangan prima. Tulis bilangan ini di daftar B, lalu coret bilangan ini dan semua kelipatannya dari daftar A.
6. Ulangi langkah 4 sampai semua bilangan di daftar A sudah tercoret.
-}
-- Contoh soal no 5 mencari bilangan prima dari 2 sampai 10
-- z `mod` x \= 0 artinya bilangan z tidak habis dibagi x, jadi bukan kelipatannya
-- Outputnya keluar, tapi ada error Non-exhaustive patterns in function sieve, mungkin karena sieve [] belum didefinisikan.


-- NO 6
pythaTriple = [(x,y,z) |  z <- [5 ..]
                        , y <- [z, z-1 .. 1]
                        , x <- [y, y-1 .. 1]
                        , x*x + y*y == z*z ]
-- segitiga pythagoras pertama adalah (3,4,5) jadi z >= 5, y pasti lebih kecil dari z dan x pasti lebih kecil dari y