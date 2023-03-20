-- Chapter 10
-- New Kinds of Data
--
-- Types
-- Compound types
-- Own types
--
-- data <Type_Name> = 
--      Constructor1
--      | Constructor2
--      | Constructor3
--      | ...
--      | Constructorn
--      deriving Show
--
--  polymorphic
--  data <Name> type_var = 
--      Constructor1
--      | Constructorn type_var
--      deriving Show
--
--  polymorphic and recursive
--  data <Name> type_var =
--      Constructor1
--      | Constructor2 type_var 
--      (Name type_var)
--      deriving Show
--

data Colour = Red | Green | Blue | Yellow deriving Show

col :: Colour
cols :: [Colour]

col = Blue
cols = [Red, Blue, Green, Yellow]
colpair = ('R', Red)

--
-- data Maybe a = Nothing | Just a deriving Show

--
data Sequence a = Nil | Cons a (Sequence a) deriving Show

s0 = Nil
s1 = Cons 1 (Cons 2 (Cons 3 Nil))
s2 = Cons 4 (Cons 5 (Cons 6 Nil))

seqLength :: Num b => Sequence a -> b
seqLength Nil = 0
seqLength (Cons _ xs) = 1 + seqLength xs

seqAppend :: Sequence a -> Sequence a -> Sequence a

seqAppend Nil ys = ys
seqAppend (Cons x xs) ys = Cons x (seqAppend xs ys)

---
--- A type for Mathematical Expressions
---

{- data Expr a =
    Num a
    | Add (Expr a) (Expr a)
    | Subtract (Expr a) (Expr a)
    | Multiply (Expr a) (Expr a)
    | Divide (Expr a) (Expr a)
    deriving Show

expExample = Add (Num 1) (Multiply (Num 2) (Num 3))

evaluate :: Integral a => Expr a -> a

evaluate (Num x) = x
evaluate (Add e e') = evaluate e + evaluate e'
evaluate (Subtract e e') = evaluate e - evaluate e'
evaluate (Multiply e e') = evaluate e * evaluate e'
evaluate (Divide e e') = evaluate e `div` evaluate e'
 -}
---
-- Questions
---

-- Q1
data Rect a = 
    Square a -- side length
    | Rectangle a a -- width, height
    deriving Show

-- Q2
area' :: Num a => Rect a -> a
area' (Square s) = s*s
area' (Rectangle w h) = w*h

-- Q3
rotate' :: Ord a => Rect a -> Rect a
rotate' (Square s) = Square s
rotate' (Rectangle w h) = 
    if w > h
    then Rectangle h w
    else Rectangle w h

-- Q4
--
rectangles :: (Num a, Ord a) => [Rect a]
rectangles = [Square 6, Rectangle 4 3, Rectangle 5 6, Square 2]

pack :: Ord a => [Rect a] -> [Rect a]
widthOfRect :: Rect a -> a
rectCompare :: (Ord a) => Rect a -> Rect a -> Bool

-- sorting function
--

-- merge :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] rs = rs
merge _ ls [] = ls
merge cmp (x:xs) (y:ys) = 
    if cmp x y
    then x : merge cmp xs (y:ys)
    else y : merge cmp (x:xs) ys

-- mergeSort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort cmp xs =
    let
        split = length xs `div` 2
        left = take split xs
        right = drop split xs
    in
        merge cmp (mergeSort cmp left) (mergeSort cmp right)
--
-- mapper
--
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) =
    f x : map' f xs
--

widthOfRect (Square s) = s
widthOfRect (Rectangle w h) = w

rectCompare a b =
    widthOfRect a < widthOfRect b

pack rects = mergeSort rectCompare (map' rotate' rects)

---
-- Q5
---
-- data Sequence a = Nil | Cons a (Sequence a) deriving Show
-- seqTake
seqTake :: Integral a => a -> Sequence b -> Sequence b
seqTake _ Nil = Nil
seqTake 0 _ = Nil
seqTake n (Cons x xs) =
    Cons x (seqTake (n-1) xs)

-- seqDrop
seqDrop :: Integral a => a -> Sequence b -> Sequence b
seqDrop _ Nil = Nil
seqDrop 0 xs = xs
seqDrop n (Cons x xs) =
    seqDrop (n-1) xs

-- seqMap
seqMap :: (a -> b) -> Sequence a -> Sequence b
seqMap _ Nil = Nil
seqMap f (Cons x xs) = 
    Cons (f x) (seqMap f xs)

---
-- Q6 
---
power :: (Num a, Integral b) => a -> b -> a
power x 0 = 1
power x n = x * power x (n-1)

data Expr a = 
    Num a
    | Add (Expr a) (Expr a)
    | Subtract (Expr a) (Expr a) 
    | Multiply (Expr a) (Expr a)
    | Divide (Expr a) (Expr a)
    | Power (Expr a) (Expr a)
    deriving Show

evaluate :: Integral a => Expr a -> a

evaluate (Num x) = x
evaluate (Add e e') = evaluate e + evaluate e'
evaluate (Subtract e e') = evaluate e - evaluate e'
evaluate (Multiply e e') = evaluate e * evaluate e'
evaluate (Divide e e') = evaluate e `div` evaluate e'
evaluate (Power e e') = power (evaluate e) (evaluate e')

---
-- Q7
---
-- evaluate2 :: Integral a => Expr a -> Maybe a

evaluate2 (Num x) = Just x
evaluate2 (Add e e') = 
    case (evaluate2 e, evaluate2 e') of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a, Just b) -> Just (a + b)
evaluate2 (Subtract e e') = 
    case (evaluate2 e, evaluate2 e') of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a, Just b) -> Just (a - b)
evaluate2 (Multiply e e') = 
    case (evaluate2 e, evaluate2 e') of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a, Just b) -> Just (a * b)
evaluate2 (Divide e e') = 
    case (evaluate2 e, evaluate2 e') of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (_, Just 0) -> Nothing
        (Just a, Just b) -> Just ( a `div` b)
evaluate2 (Power e e') = 
    case (evaluate2 e, evaluate2 e') of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a, Just b) -> Just (power a b)

exp0 = Divide (Num 2) (Num 0)
exp1 = Divide (Num 2) (Num 1)
exp2 = Divide (Num 2) (Num 2)
exp3 = Divide (Num 2) (Num 3)
eval = map' evaluate2 [exp0, exp1, exp2, exp3]
