-- Chapter 5
-- Sorting Things
--


---------------------------------------------------------------------
-- INSERTION SORT
---------------------------------------------------------------------
insert :: Ord a => a -> [a] -> [a]

insert x [] = [x]               -- an element and an empty list is just a sorted list with the element inserted in the empty list
insert x (y:ys) =               -- otherwise the list will have a head and a tail
    if x <= y                   -- IF the element is in the correct position compared to the head
        then x : y : ys         -- THEN cons the element to the list
        else y : insert x ys    -- OTHERWISE keep head and carry on
                                -- Note, insert x (y:ys) = y : insert x ys.
                                -- That is, insert and cons are commutative. 
                                -- Problem size is reduced.

sort :: Ord a => [a] -> [a]

sort [] = []                        -- an empty list is already sorted
sort (x:xs) = insert x (sort xs)    -- insert the head into the sorted tail.
-- The sort function deconstructs the list into a composition of inserts, ending with the nil list.
-- Then starting from the inner most insert the list is reconstructed.
-- At each stage, the accumulating, ordered list may be deconstructed 
-- until the element that needs to be inserted is in the correct postion
-- when compared to the head of the current accumulated ordered list.

---------------------------------------------------------------------
-- Merge
---------------------------------------------------------------------
merge :: Ord a => [a] -> [a] -> [a]

merge [] l = l                      -- IF the first list is empty THEN the second list is already sorted, by assuption.
merge l [] = l                      -- IF the second list is empty THEN the first list is already sorted, by assuption.
merge (x:xs) (y:ys) =
    if x < y
        then x : merge xs (y:ys)    -- keep the head of the first list and sort the tail of the first with the second list
        else y : merge (x:xs) ys    -- keep the head of the second list and sort the tail of the second with the first list

-- ASSUPTIONS: the two input lists are sorted.

---------------------------------------------------------------------
-- MERGESORT
---------------------------------------------------------------------

-- function to get length of list
length1 :: Integral b => [a] -> b

length1 [] = 0
length1 (_:xs) = 1 + length1 xs

-- return a list of the first n elements of a list
take1 :: (Eq a, Integral a) => a -> [b] -> [b]

take1 0 _ = []
take1 n (x:xs) = x : take1 (n-1) xs

-- return a list after dropping the last n elements of the list
drop1 :: (Eq a, Integral a) => a -> [b] -> [b]

drop1 0 xs = xs
drop1 n (_:xs) = drop1 (n-1) xs

-- split list in two then recursive call on each half to sort then merge
mergeSort :: Ord a => [a] -> [a]

mergeSort [] = []                                       -- an empty list is sorted
mergeSort [x] = [x]                                     -- a singleton list is sorted
mergeSort l =
    let left = take1 (length1 l `div` 2) l              -- get the left half of the list
        right = drop1 (length l `div` 2) l              -- get the right half of the list
    in
        merge (mergeSort left) (mergeSort right)        -- sort sublists and merge
-- Merge sort deconstructs the list into nested halves, with the stack being in a binary tree structure
-- 1. The lists are deconstructed using `take` and `drop` until the bases case are reached 
-- The binary tree is collapse back into an ordered list
-- 2. Since the base cases are sorted lists, able to use the merge function.

-- using the `where` construct
mergeSort2 :: Ord a => [a] -> [a]

mergeSort2 [] = []                                    -- an empty list is sorted
mergeSort2 [x] = [x]                                  -- a singleton list is sorted
mergeSort2 l =
    merge (mergeSort2 left) (mergeSort2 right)        -- sort sublists and merge
        where
            left = take1 (length1 l `div` 2) l        -- get the left half of the list
            right = drop1 (length l `div` 2) l        -- get the right half of the list


---------------------------------------------------------------------
-- QUESTIONS
---------------------------------------------------------------------

-- Q1
mergeSort3 :: Ord a => [a] -> [a]

mergeSort3 [] = []                              -- an empty list is sorted
mergeSort3 [x] = [x]                            -- a singleton list is sorted
mergeSort3 l =
    merge (mergeSort3 left) (mergeSort3 right)  -- sort sublists and merge
        where
            len = length1 l `div` 2             -- get the splitting length
            left = take1 len l                  -- get the left half of the list
            right = drop1 len l                 -- get the right half of the list

-- Q3
insert2 :: Ord a => a -> [a] -> [a]
sort2 :: Ord a => [a] -> [a]

insert2 x [] = [x]
insert2 x (y:ys) =
    if x >= y
        then x : y : ys
        else y : insert2 x ys

sort2 [] = []
sort2 (x:xs) = insert2 x (sort2 xs)

-- Q4
-- to check if the list is sorted
sortedDown :: Ord a => [a] -> Bool

sortedDown [] = True
sortedDown [x] = True
sortedDown (x:y:xs) =
    if x >= y
    then sortedDown (y:xs)
    else False

sortedUp :: Ord a => [a] -> Bool

sortedUp [] = True
sortedUp [x] = True
sortedUp (x:y:xs) =
    if x >= y
    then sortedUp (y:xs)
    else False

-- Q5
-- `<` on lists 
-- (<) [] [] = False                                            if both lists are empty the one cannot be smaller than the other
-- (<) [x] [] = False                                           if a list with an element will always be bigger than an empty list
-- (<) [] [x] = True
-- (<) x:xs y:ys = if x == y then xs `<` ys else x `<` y        if both list are nonempty then both have heads
--                                                              if the heads are equal then compare the tails
--                                                              otherwise determine order by value of head

-- Q6
-- using `where` on insert sort
sortComplete1 :: Ord a => [a] -> [a]

sortComplete1 [] = []
sortComplete1 (x:xs) = insert x (sortComplete1 xs)
    where
        insert x [] = [x]
        insert x (y:ys) = 
            if x < y
                then x : y : ys
                else y : insert x ys

-- using `let` on insert sort
sortComplete2 :: Ord a => [a] -> [a]

sortComplete2 [] = []
sortComplete2 (x:xs) = 
    let
        insert x [] = [x]
        insert x (y:ys) = 
            if x < y
                then x : y : ys
                else y : insert x ys
    in
        insert x (sortComplete2 xs)

