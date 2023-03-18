-- Chapter 9
-- More with Functions
--

-- `Num a => a -> a -> a` is acutally `Num a => a -> (a -> a)`
-- `->` is a right-associative operator 
--
-- partial application
--
-- Summary
-- f x y has type a -> (b -> c) this is equivalent to a -> b -> c
-- Since, in the language of type, `->` is right-associative operator 
--- f takes an argument of type `a` and returns a function of type (b -> c)
--- the partial function takes an argument of type `b` and a value of type c
-- `f x y = ...` is syntatic sugar for `f = \x -> \y -> ...`
--
-- QUESTIONS
--
-- Q1
-- Consider the three argument function `g a b c`
-- type is: a -> (b -> (c -> d))
--          a -> (b -> c -> d)      right-associative
--          a -> b -> c -> d        right-associative
-- f takes an argument of type `a` and returns a partial function of type (b -> (c -> d))
-- then the partial function takes an argument of type `b` and returns a partial function of type (c -> d)
-- then the paritial function takes an argument of type `c` and returns a value of type `d`
--
-- Partial application: can apply just one or two arguments to the function g
--
-- g a b c = ... is syntatic sugar for g = \a -> \b -> \c -> ...
--
-- Q2
-- Check is an element exists within a list
elem' :: Eq a => a -> [a] -> Bool

elem' _ [] = False
elem' x (y:ys) =
    if x == y
    then True
    else elem' x ys

-- negate the boolean value
not' :: Bool -> Bool
not' x = x == False


-- partial application
-- elem' e :: Eq a => [a] -> Bool

map' :: (a -> b) -> [a] -> [b]

map' _ [] = []
map' f (x:xs) =
    f x : map' f xs


-- check if a given element exists in every list within a list
-- version 1
--
elemAll1 :: Eq a => a -> [[a]] -> Bool

elemAll1 e ls = 
    elem' False bools == False
    where bools = map' (elem' e) ls

-- version 2
elemAll2 :: Eq a => a -> [[a]] -> Bool

elemAll2 e lls =
    not' (elem' False (map' (elem' e) lls))

-- version 3 
elemAll3 :: Eq a => a -> [[a]] -> Bool

elemAll3 e =
    not' . (elem' False) . (map' (elem' e))

--
-- Q3
-- maps a function over lists of lists of lists
mapll :: (a -> b) -> [[[a]]] -> [[[b]]]

-- version 1
mapll1 f xsss = map' (map' (map' f)) xsss

-- version 2
mapll2 f = map' (map' (map' f))

-- version 3
mapll = map' . map' . map'


--
-- Q4
--
-- trancates a list to a given length or less
-- version 1
truncateList :: Integral n => n -> [[a]] -> [[a]]
truncateL :: Integral n => n -> [a] -> [a]

truncateL _ [] = []
truncateL 0 xs = []
truncateL n (x:xs) = x : truncateL (n-1) xs

truncateList n = map' (truncateL n)


--
-- Q5
-- given a list of lists returns a list of heads
firstElts :: Num a => a -> [[a]] -> [a]
firstElt :: Num a => a -> [a] -> a

firstElt x [] = x
firstElt _ (x:_) = x

firstElts = map' . firstElt

--
-- Q6
-- operator section to put a number onto the front of all the lists in a list of list numbers
appendNum :: Num a => a -> [[a]] -> [[a]]

-- version 1
appendNum1 n xss = map' (n:) xss

-- version 2
appendNum n = map' (n:)
