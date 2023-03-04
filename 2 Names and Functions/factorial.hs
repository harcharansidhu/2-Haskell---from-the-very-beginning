factorial :: (Eq a, Num a) => a -> a

factorial n =
  if n == 1
  then 1
  else n * factorial (n-1)
