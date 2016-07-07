module JoinList where

import Data.Monoid

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Excercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = let t1 = tag jl1
                  t2 = tag jl2
               in Append (t1 <> t2) jl1 jl2

-- Excercise 2
-- (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a)                   = Just a
indexJ i (Append _ jl1 jl2) | i < s1    = indexJ i jl1
                            | otherwise = indexJ (i - s1) jl2
                            where s1 = getSize $ size $ tag jl1
indexJ _ _                              = Nothing

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


test2 :: [Int] -> [Bool]
test2 = map (\i-> indexJ i exjl3 == (jlToList exjl3 !!? i))

test_indexJ :: Int -> JoinList m a -> Maybe a
test_indexJ i jl =  jlToList jl !!? i

-- jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
dropJ i jl                 | i <= 0    = jl
                           | i >= s    = Empty
                            where s = getSize $ size $ tag jl
dropJ i (Append _ jl1 jl2) | i > s1    = dropJ (i-s1) jl2
                           | otherwise = dropJ i jl1 +++ jl2
                            where s1 = getSize $ size $ tag jl1
dropJ _ _                              = Empty

test22 :: [Bool]
test22 = map (\i -> jlToList (dropJ i exjl3) == drop i (jlToList exjl3)) [-1..5]

-- jlToList (takeJ n jl) == take n (jlToList jl)
takeJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
takeJ i jl | i <= 0 = Empty
           | i >= s = jl
            where s = getSize $ size $ tag jl
takeJ _ Empty = Empty
takeJ 1 jl@(Single _ _) = jl
takeJ i (Append _ jl1 jl2) | i <= s1   = takeJ i jl1
                           | otherwise = jl1 +++ takeJ (i-s1) jl2
  where s1 = getSize $ size $ tag jl1

test23 :: [Bool]
test23 = map (\n -> jlToList (takeJ n exjl3) == take n (jlToList exjl3)) [-1..6]

----------
exjl1 :: JoinList (Product Integer) Char
exjl1 =
  Append (Product 210)
    (Append (Product 30)
      (Single (Product 5) 'y')
      (Append (Product 6)
        (Single (Product 2) 'e')
        (Single (Product 3) 'a')))
    (Single (Product 7) 'h')

exjl2 :: JoinList (Product Integer) Char
exjl2 = Single (Product 5) 'd'

exjl3 :: JoinList Size Char
exjl3 =
  Append 4
    (Append 3
      (Single 1 'y')
      (Append 2
        (Single 1 'e')
        (Single 1 'a')))
    (Single 1 'h')
