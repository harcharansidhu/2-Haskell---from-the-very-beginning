-- Chapter 7
-- When Things Go Wrong
--

-- type `Maybe a` can hold `Nothing`
x :: Maybe a

x = Nothing

-- or can hold `just` something of type `a`
y :: Maybe Char

y = Just 'x'

--
--
z :: Num a => Maybe [a]

z = Just [1, 2]

-- `Maybay` allows the program to handle all cases
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]

safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail [] = Nothing
safeTail (_:xs) = Just xs

---
safeDiv :: Integral a => a -> a -> Maybe a

safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

---
-- safeSlice :: (Num a, Ord a) => (a -> [b] -> [b]) -> a -> [b] -> Maybe [b]
safeTake ::  Int -> [a] -> Maybe [a]
safeDrop ::  Int -> [a] -> Maybe [a]

safeSlice f n l =
    if n >= 0 && n <= length l
    then Just (f n l)
    else Nothing

safeTake n l = safeSlice take n l
safeDrop n l = safeSlice drop n l

---
-- Using `case ... of ...` to provide the possiblility that the mapping may yeild `Nothing` for some elements
mapMayBe :: (a -> Maybe b) -> [a] -> [b]

mapMayBe _ [] = []
mapMayBe f (x:xs) =
    case f x of
        Nothing -> rs
        Just r -> r : rs
    where
        rs = mapMayBe f xs

---
-- If you wish to carry information about the failure
-- which led to no result being produced for a given input
-- `Either`
--   `Left`: error results
--   `Right`: successful results 

safeDiv2 :: (Integral a) => a -> a -> Either String a

safeDiv2 _ 0 = Left "Division by Zero"
safeDiv2 x y = Right (x `div` y)

---
-- Questions
--- 

---
-- Q1
---
filterPositive :: (Num a, Ord a) => [a] -> [a]
smaller :: (Num a, Ord a) => [a] -> Maybe a
smallest :: (Num a, Ord a) => [a] -> Maybe a

filterPositive [] = []
filterPositive (x:xs) =
    if x > 0
    then x : filterPositive xs
    else filterPositive xs

smaller [] = Nothing
smaller [x] = 
    if x > 0
    then Just x
    else smaller []
smaller (x:y:ys) = 
    if x < y
    then smaller (x:ys)
    else smaller (y:ys)

smallest = smaller.filterPositive

---
-- Q2
---
smallest0 :: (Num a, Ord a) => [a] -> a

smallest0 xs =
    case smallest xs of
        Nothing -> 0
        Just x -> x

---
-- Q3
---
sqrtMaybe :: (Num a, Ord a) => a -> Maybe a
sqrtFloor :: (Num a, Ord a) => a -> a -> a

sqrtFloor n x 
    | n*n < x = sqrtFloor (n+1) x
    | n*n == x = n
    | otherwise = n - 1

sqrtMaybe x =
    if x < 0
    then Nothing
    else Just (sqrtFloor 0 x)

---
-- Q4
---
mapMaybe0 :: (a -> Maybe b) -> b -> [a] -> [b]

mapMaybe0 _ _ [] = []
mapMaybe0 f d (x:xs) =
    case f x of
        Nothing -> d:rs
        Just r -> r:rs
    where
        rs = mapMaybe0 f d xs

---
-- Q5
---
splitEither :: (a -> Either b c) -> [a] -> ([b], [c])

splitEither _ [] = ( [], [] )
splitEither fn (x:xs) =
    let (ls, rs) = splitEither fn xs in
        case fn x of
            Left l -> ( l:ls, rs )
            Right r -> ( ls, r:rs)

