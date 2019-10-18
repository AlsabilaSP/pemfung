-- 1. (*) The Maximum Function
-- maxi x y returns the maximum of x and y
-- Belajar |
maxi x y | x >= y = x | otherwise = y

-- 2. Sum of squares.
-- sumsq n returns 1*1 + 2*2 + ... + n*n
-- sumsq n = sum [ x*x | x <- [1..n] ] pakai list comprehension
-- sumsq n = sum (map (\x -> x*x) [1..n]) pakai higher order function
sumsq 0 = 0
sumsq n | n > 0 = sumsq (n-1) + n*n

-- 4. Fibonacci Numbers
-- fib n computes the nth Fibonacci number
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- 6. Defining Types
-- a datatype for months
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
 deriving ( Eq, Show )
{-
The last line – “deriving Show” – tells the system to build a show function for the type Shape (more on this later).
-}
-- daysInMonth month year returns the days in the month in the year
daysInMonth :: Month -> Integer -> Integer
daysInMonth January   year = 31
daysInMonth February  year
  | leap year              = 29
  | otherwise              = 28
daysInMonth March     year = 31
daysInMonth April     year = 30
daysInMonth May       year = 31
daysInMonth June      year = 30
daysInMonth July      year = 31
daysInMonth August    year = 31
daysInMonth September year = 30
daysInMonth October   year = 31
daysInMonth November  year = 30
daysInMonth December  year = 31

-- leap year approximates whether year is a leap year
leap :: Integer -> Bool
leap year = year `mod` 4 == 0

-- a datatype for dates
data Date = Date{ year :: Integer, month :: Month, day :: Integer }

-- validDate date checks whether date is a valid date, regarding
-- numbers of days in the month
validDate :: Date -> Bool
validDate date = 1 <= day date
                   && day date <= daysInMonth (month date) (year date)

-- 7. Replication
repli :: Integer -> String -> String
repli 0 str = ""
repli n str = str ++ repli (n-1) str

-- 8. Muliplying list elements
-- multiply xs multiplies all elements in the list xs
multiply :: Num a => [a] -> a
multiply []     = 1
multiply (x:xs) = x * multiply xs
-- alternative dengan foldr
-- multiply xs = foldr (*) 1 xs

-- 9. Avoiding Duplicates
-- duplicates xs checks if there are duplicates in the list xs
duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates (x:xs) = exists x xs || duplicates xs

-- exists x xs checks whether x exists as an element in the list xs
exists :: Eq a => a -> [a] -> Bool
exists x []     = False
exists x (y:ys) = x == y || exists x ys