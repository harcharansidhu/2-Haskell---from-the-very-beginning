{- 
 - Haskell the very beggining
 - john Whitington
 - Chapter 3
 - Case by Case
 -}

-- Pattern Matching

--
-- Factorial Function
--

fact1 :: (Eq a, Num a) => a -> a

fact1 n =
  if n == 1
  then 1
  else n * fact1 (n-1)


fact2 :: (Eq a, Num a) => a -> a

fact2 1 = 1
fact2 n = n * fact2 (n-1)

--
-- Vowels
--

isVowel1 :: Char -> Bool

isVowel1 c = 
  c == 'a' ||
  c == 'e' ||
  c == 'i' ||
  c == 'o' ||
  c == 'u' 

isVowel2 :: Char -> Bool

isVowel2 'a' = True
isVowel2 'e' = True
isVowel2 'i' = True
isVowel2 'o' = True
isVowel2 'u' = True
isVowel2 _ = False

--
-- Euclids Algorithm
--

gcd1 :: Integral a => a -> a -> a

gcd1 a b =
  if b == 0
  then a
  else gcd1 b (a `rem` b)

gcd2 :: Integral a => a -> a -> a

gcd2 a 0 = a
gcd2 a b = gcd2 b (a `rem` b)

--
-- Guarded Equations
--
-- Sign of a number
--

sign1 :: (Ord a, Num a, Integral b) => a -> b

sign1 x = 
  if x < 0 then -1
  else if x > 0 then 1
  else 0

sign2 :: (Ord a, Num a, Integral b) => a -> b

sign2 x 
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

isVowel3 c 
  | c == 'a' = True
  | c == 'e' = True
  | c == 'i' = True
  | c == 'o' = True
  | c == 'u' = True
  | otherwise = False

--
-- Questions
-- page 27

--
--Question 1
--
not1 :: Bool -> Bool

not1 True = False
not1 False = True

--
-- Question 2
--
sumMatch1 :: (Eq a, Integral a) => a -> a

sumMatch1 0 = 0
sumMatch1 n = n + sumMatch1 (n-1)

--
-- Question 3
--
power1 :: (Num a, Integral b, Eq b) => a -> b -> a

power1 x 0 = 1
power1 x n = x * power1 x (n-1)

--
-- Question 4
--
not2 x 
  | x == True = False
  | otherwise = True

sumMatch2 n  
  | n == 0 = 0
  | otherwise = n + sumMatch2 (n-1)

power2 x n 
  | n == 0 = 1
  | otherwise = x * power2 x (n-1)

--
-- Question 4
--
charCategory :: (Integral a) => Char -> a 

charCategory c 
  | ('a' <= c) && (c <= 'z') = 0
  | ('A' <= c) && (c <= 'Z') = 1
  | otherwise = 2
