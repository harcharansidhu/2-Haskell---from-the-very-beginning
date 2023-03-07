-- function to check if the list is empty
isNil :: [a] -> Bool

isNil [] = True
isNil _ = False

-- function to get length of list
length' :: Integral b => [a] -> b

length' [] = 0
length' (_:xs) = 1 + length' xs

-- function to sum the values in the list
sumElts :: Num a => [a] -> a

sumElts [] = 0
sumElts (x:xs) = x + sumElts xs

-- return the a list of odd indexed elements
oddElements :: [a] -> [a]

oddElements [] = []
oddElements [x] = [x]
oddElements (x:_:xs) = x : oddElements xs

-- version 2: return the a list of odd indexed elements
oddElements2 :: [a] -> [a]

oddElements2 (x:_:xs) = x : oddElements xs
oddElements2 xs = xs

-- implement the ++ operator
append :: [a] -> [a] -> [a]

append [] ys = ys
append (x:xs) ys = x : append xs ys

-- reverse the list
reverse1 :: [a] -> [a]

reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

-- return a list of the first n elements of a list
take1 :: (Eq a, Integral a) => a -> [b] -> [b]

take1 0 _ = []
take1 n (x:xs) = x : take1 (n-1) xs

-- return a list after dropping the last n elements of the list
drop1 :: (Eq a, Integral a) => a -> [b] -> [b]

drop1 0 xs = xs
drop1 n (_:xs) = drop1 (n-1) xs

-- QUESTIONS

------------------------------------------------
-- Q1
------------------------------------------------
-- Return the even indexed elements
------------------------------------------------
evenElements :: [a] -> [a]

evenElements [] = []
evenElements [x] = []
evenElements (_:x:xs) = x : evenElements xs

------------------------------------------------
-- Q2
------------------------------------------------
-- Return the number of True elements in the list
------------------------------------------------
countTrue1 :: Integral b => [Bool] -> b

countTrue1 [] = 0
countTrue1 (x:xs)
  | x == True = 1 + countTrue1 xs
  | otherwise = countTrue1 xs
  

countTrue2 :: Integral b => [Bool] -> b

countTrue2 [] = 0
countTrue2 (True : xs) = 1 + countTrue2 xs
countTrue2 (False : xs) = countTrue2 xs

------------------------------------------------
-- Q3
------------------------------------------------
-- palindrome on a list
------------------------------------------------
list2Palindrome :: [a] -> [a]

list2Palindrome xs = xs ++ reverse1 xs

------------------------------------------------
-- check if list is a palindrome
------------------------------------------------
isPalindrome :: (Eq a) => [a] -> Bool

isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = 
  let y:ys = reverse1 xs in 
  if x == y then isPalindrome ys else False

------------------------------------------------
-- Q4
------------------------------------------------
-- returns a list without the last element
------------------------------------------------
dropLast :: [a] -> [a]

dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs

------------------------------------------------
-- Q5
------------------------------------------------
-- checks is an element exits in a list
------------------------------------------------
elem' :: Eq a => a -> [a] -> Bool

elem' x [] = False
elem' x (y:ys) = if x == y then True else elem' x ys


------------------------------------------------
-- Q6
------------------------------------------------
-- makes a set from a list
------------------------------------------------
makeSet :: Eq a => [a] -> [a]

makeSet [] = []
makeSet [x] = [x]
makeSet (x:xs) = if elem' x xs then makeSet xs else x : makeSet xs

------------------------------------------------
-- Q8
------------------------------------------------
-- make a reverse enumerated list
------------------------------------------------
q8 = reverse1 [1 .. 10]

------------------------------------------------
-- Q9
------------------------------------------------
-- filter a list with factors of 21 and 83
------------------------------------------------
q9a = [x | x <- [1 .. 9999], x `rem` 21 == 0 && x `rem` 83 == 0]

q9b = [x | x <- [1 .. 9999], x `rem` 21 == 0 || x `rem` 83 == 0]

------------------------------------------------
-- Q10
------------------------------------------------
-- use list comprehension on Q2
------------------------------------------------
countTrue3 :: Integral a => [Bool] -> a

countTrue3 xs = 
    let
        count [] = 0
        count (y:ys) = 1 + count ys
    in
        count [x | x <- xs, x == True]
     
