module Main where

import Test.HUnit

import Calc
import ExprT

testEx1 :: Test
testEx1 = test [eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) ~?= 20]

testEx3 :: Test
testEx3 = test [(reify $ mul (add (lit 2) (lit 3)) (lit 4)) ~?= Mul (Add (Lit 2) (Lit 3)) (Lit 4)]

testEx6 :: Test
testEx6 = test [ (withVars [("x", 6)] $ add (lit 3) (var "x")) ~?= Just 9
               , ( withVars [("x", 6)] $ add (lit 3) (var "y") ) ~?= Nothing
               , ( withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x")) ) ~=? Just 54
               ]

tests :: [Test]
tests = [ testEx1
        , testEx3
        , testEx6
        ]

main :: IO ()
main = runTestTT (TestList tests) >> return ()