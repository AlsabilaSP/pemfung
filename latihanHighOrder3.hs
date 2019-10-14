-- 3. Generating Lists

-- Saya tidak bisa import Test.QuickCheck :') jadi tidak bisa mengejakan soal 1 dan 2

-- NO 3
prop_ZipUnzip :: [(Int,Int)] -> Bool
prop_ZipUnzip xys =
  zip xs ys == xys
 where
  (xs,ys) = unzip xys

-- Ingat zip :: [a] -> [b] -> [(a,b)]
-- Ingat unzip :: [(a,b)] -> ([a], [b])
-- Fungsi prop_ZinUnzip mengambil input xys bertipe [(Int,Int)], outputnya adalah nilai kebenaran dari zip xs ys == xys dimana (xs,yx) adalah output dari unzip xys
-- xys bertipe [(a,b)], unzip xys akan mengeluarkan ([a],[b]), xs = [a] dan ys = [b]
-- zip xs ys atau zip [a] [b] akan mengeluarkan [(a,b)], dimana [(a,b)] == xys
-- Jadi, seharusnya outputnya True