--http://www.seas.upenn.edu/~cis194/spring13/hw/06-laziness.pdf
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Prelude hiding ((/))

-- Exercise 1
fib :: Integer -> Integer
fib n | n <= 1    = n
      | otherwise = fib(n-1) + fib(n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = showFirst 20
    where
      showFirst :: Show a => Int -> Stream a -> String
      showFirst 0 _ = "..."
      showFirst n (Cons x xs) = show x ++ " : " ++ showFirst (n-1) xs ++ " "

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

example :: Stream Integer
example = genStream 1
  where
    genStream seed = Cons seed $ genStream (seed +1)

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed trans seed = Cons seed $ streamFromSeed trans (trans seed)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

zeros :: Stream Integer
zeros = streamFromSeed id 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x $ Cons y $ interleaveStreams xs ys

ruler :: Stream Integer
ruler = let Cons _ positive = nats
        in interleaveStreams zeros positive

-- Exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 zeros

smap :: (a -> b) -> Stream a -> Stream b
smap f (Cons y ys) = Cons (f y) $ smap f ys

instance Num (Stream Integer) where
  fromInteger n = Cons n zeros

  negate (Cons z zs) = Cons (-z) $ negate zs

  (Cons z zs) + (Cons y ys) = Cons (z + y) $ zs + ys

  (Cons a0 a') * b@(Cons b0 b') = Cons (a0 * b0) (a0b' + a' * b)
    where
      a0b' = smap (* a0) b'

-- instance Fractional (Stream Integer) where
(/) :: Stream Integer -> Stream Integer -> Stream Integer
(/) (Cons a0 a') (Cons b0 b') = q
  where
    q = Cons (a0 `div` b0) $ smap (`div` b0) (a' - q*b')


fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer deriving (Show, Eq)

instance Num Matrix where
    (Matrix a1 b1 c1 d1) * (Matrix a2 b2 c2 d2) = Matrix (a1*a2 + b1*c2) (a1*b2 + b1*d2)
                                                         (b1*a2 + d1*c2) (c1*b2 + d1*d2)

fib4 :: Integer -> Integer
fib4 n | n <= 1    = n
       | otherwise = let (Matrix a _ _ _) = (Matrix 1 1 1 0)^(n-1) in a