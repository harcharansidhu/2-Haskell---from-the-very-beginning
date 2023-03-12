-- Chapter 6
-- Functions on functions on functions
--

-- *** PATTERN ***
-- Apply a function to every element of a list
-- Use recursion

-- Doubling each elemtn in the list
doubleList :: Num a => [a] -> [a]

doubleList [] = []
doubleList (x:xs) = 2*x : doubleList xs

-- Checks if the number is even in the list
evens :: (Eq a, Integral a) => [a] -> [Bool]

evens [] = []
evens (x:xs) = (x `rem` 2 == 0) : evens xs

-- function as an arguement
map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) = f x : map' f xs

-- Using map on isEven
isEven :: Integral a => a -> Bool
evens1 :: Integral a => [a] -> [Bool]

isEven x = (x `rem` 2) == 0
evens1 xs = map' isEven xs

-- using anonymous function
even2 :: Integral a => [a] -> [Bool]

even2 xs =
    map' (\x -> x `rem` 2 == 0) xs

-- use a comparison function on MERGE SORT
merge :: (Ord a) => (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]

merge _ [] xs = xs
merge _ xs [] = xs
merge cmp (x:xs) (y:ys) =
    if cmp x y
        then x : merge cmp xs (y:ys)
        else y : merge cmp (x:xs) ys

mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort cmp xs = 
    merge cmp (mergeSort cmp ls) (mergeSort cmp rs)
    where
        split = (length xs) `div` 2
        ls = take split xs
        rs = drop split xs

--
-- Questions
--

-- Q1
calm :: [Char] -> [Char]

calm [] = []
calm (x:xs) = 
    if x == '!'
        then '.' : calm xs
        else x : calm xs


calm1 :: [Char] -> [Char]

calm1 xs = map' (\x -> if x == '!' then '.' else x) xs

-- Q2
clip :: (Num a, Ord a) => a -> a

clip x 
    | x < 1 = 1
    | x > 10 = 10
    | otherwise = x

clipList :: (Num a, Ord a) => [a] -> [a]

clipList = map' clip

-- Q3
clipList1 :: (Num a, Ord a) => [a] -> [a]

clipList1 = 
    map' 
        (\x -> 
            if x < 1 then 1 
            else if x > 10 then 10 
            else x)

-- Q4
-- will return the cumulative effect of 
-- repeatedly applying the a given funtion
-- for a set number of times
apply :: (Integral b) => (a -> a) -> b -> a -> a

apply _ 0 x = x
apply f n x = (f . apply f (n-1)) x

-- Q5
-- Change INSERTION SORT to accept a comparison function
insertSort :: (a -> a -> Bool) -> [a] -> [a]
insert :: (a -> a -> Bool) -> a -> [a] -> [a]

insert _ x [] = [x]
insert cmp x (y:ys) = 
    if cmp x y
    then x : y : ys
    else y : (insert cmp x ys)

insertSort _ [] = []
insertSort cmp (x:xs) = insert cmp x (insertSort cmp xs)

-- Q6 
-- filter function to filter a list according to a boolean function
filter' :: (a -> Bool) -> [a] -> [a]

filter' _ [] = []
filter' f (x:xs) =
    if f x
    then x : filter' f xs
    else filter' f xs

-- Q7
-- a function to check if all the elements in the list satisfy a given condition
all' :: (a -> Bool) -> [a] -> Bool

all' _ [] = True
all' f (x:xs) =
    if f x
    then all' f xs
    else False

-- Q8
-- to map over nested lists
mapl :: (a -> b) -> [[a]] -> [[b]]

mapl _ [] = []
mapl f (l:ls) = map' f l : mapl f ls

-- Q9
-- reverse . filter

reversedSort15 :: (Integral a) => [a] -> [a]

reversedSort15 = reverse . mergeSort (<) . filter' (\x -> x `rem` 15 == 0)

