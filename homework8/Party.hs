{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree
import System.Environment

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty                        = GL [] 0
  mappend (GL ll fl) (GL lr fr) = GL (ll ++ lr) (fl + fr)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f acc (Node r cs) = f r $ map (treeFold f acc) cs

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)]
  -> (GuestList, GuestList)
nextLevel boss bests = (newWith, newWithout)
  where
    (with, without) = unzip bests
    newWithout = mconcat with
    newWith  = glCons boss $ mconcat without

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ treeFold nextLevel mempty t

-- Exercise 5
parse :: GuestList -> String
parse (GL empl0 fun) = "Fun score : "
                    ++ show fun ++ "\n"
                    ++ unlines (map empName empl0)
main :: IO ()
main = getArgs
    >>= readFile . head
    >>= putStrLn . parse . maxFun . read
