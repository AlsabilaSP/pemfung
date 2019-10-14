-- Lazy Evaluation

-- NO 1
{-
1. Ekspresi mengambil input list x = [1,2,3,4] dan list y = [2,3,4]
2. Melakukan penjumlahan tiap elemen x+y untuk x > y, berarti pasangan (3,2),(4,2),(4,3)
3. Hasilnya dimasukkan ke dalam list output 
-}

-- NO 2
divisor n = [ x | x <- [1..n], n `mod` x == 0 ]