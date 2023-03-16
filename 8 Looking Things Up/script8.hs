-- Chapter 8
-- Looking Things Up
--
-- Dictionary: key-value pairs
-- pairs
p :: (Num a, Num b) => (a, b)
p = (1 ,2)

q :: Num a => (a, Char)
q = (1, '1')

fst' :: (a, b) -> a
snd' :: (a, b) -> b

fst' (x, _) = x
snd' (_, y) = y

-- Create a dictionary as a list of pairs
census :: [(Integer, Char)]
census = [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd')]


-- look up operation on a disctionary
lookup' :: Eq a => a -> [(a, b)] -> Maybe b

lookup' _ [] = Nothing
lookup' k' ((k, v):xs) =
    if k' == k
    then Just v
    else lookup' k' xs

-- insert on a dictionary
add :: Eq a => a -> b -> [(a, b)] -> [(a, b)]

add k v [] = [(k, v)]
add k v ((k', v'):xs) =
    if k == k'
    then (k, v) : xs
    else (k', v') : add k v xs

-- removing a pair from a dictionary
remove :: Eq a => a -> [(a, b)] -> [(a, b)]

remove _ [] = []
remove k' ((k, v):xs) =
    if k' == k
    then xs
    else (k, v) : remove k' xs

-- check if key exists in the dictionary
keyExists :: (Eq a, Eq b) => a -> [(a, b)] -> Bool

keyExists k ds =
    lookup' k ds /= Nothing

---
-- QUESTIONS
---

-- Q1
dictLen :: [(a,b)] -> Int

dictLen [] = 0
dictLen (d:ds) = 1 + dictLen ds

-- Q2
replace :: Eq a => a -> b -> [(a, b)] -> Maybe [(a, b)]

replace k v [] = Nothing
replace k v ((k', v'):ds) =
    if k == k'
    then Just ((k, v): ds)
    else case replace k v ds of
        Just xs -> Just ((k, v) : xs)
        Nothing -> Nothing

-- Q3
-- build a dictionary from 2 lists
-- if unequal return Nothing
-- assume no duplicate keys
dictBuild :: [a] -> [b] -> Maybe [(a, b)]

dictBuild [] [] = Just []
dictBuild _ [] = Nothing
dictBuild [] _ = Nothing
dictBuild (k:ks) (v:vs) =
    case dictBuild ks vs of
        Just ds -> Just ((k, v) : ds)
        Nothing -> Nothing
 
-- Q4
dictSplit :: [(a,b)] -> ([a], [b])

dictSplit [] = ([], [])
dictSplit ((k, v) : xs) = (k:ks, v:vs)
    where (ks, vs) = dictSplit xs

-- Q5
listToDict :: Eq a => [(a, b)] -> [(a, b)]
filterDuplicatePairs :: Eq a => a -> [(a, b)] -> [(a, b)]

-- trim a pair of lists to the same length
trimToSameLength [] [] = ([], [])
trimToSameLength [] _ = ([], [])
trimToSameLength _ [] = ([], [])
trimToSameLength (x:xs) (y:ys) = (x:ls, y:rs)
    where (ls, rs) = trimToSameLength xs ys

-- helper function
-- given a key remove duplicate key-value pairs
filterDuplicatePairs k [] =[]
filterDuplicatePairs k ((k1,v1):ps) =
    if k == k1
    then filterDuplicatePairs k ps
    else (k1, v1) : (filterDuplicatePairs k ps)

listToDict [] = []
listToDict [(k, v)] = [(k, v)]
listToDict ((k, v) : ps) =
    (k, v) : (listToDict xs)
    where
        xs = filterDuplicatePairs k ps

-- Q5
-- Union of two dictionaries
-- preference to the first dictionary in duplicates
joinDicts :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
nElem :: Eq a => a -> [(a,b)] -> Bool

nElem x [] = True
nElem x ((k,v) : ys) =
    if x == k
    then False
    else nElem x ys


joinDicts [] [] = []
joinDicts [] ds = ds
joinDicts ds [] = ds
joinDicts ds ((k,v) : ps) =
    if nElem k ds 
    then (k,v) : rs
    else rs
        where rs = joinDicts ds ps


