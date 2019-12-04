-- MENCOBA JAWAB SOAL KUIS

-- NO 1 
kpk x y = x*y `div` gcd x y
{-
Alternative
kpk x y = lcm x y
wkwkwk
-}


-- NO 4
maxList [] = 0
maxList xs = foldr (maximus) 0 xs
 where maximus x y | x >= y = x | otherwise = y
{-
Alternative supaya lebih efektif
maxList (x:xs) = foldr (maximus) x xs
 where maximus x y | x >= y = x | otherwise = y
-}
