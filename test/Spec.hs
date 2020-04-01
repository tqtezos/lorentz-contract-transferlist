module Main where

import Test.Tasty
import Tree (tests)

main :: IO ()
main = do
  putStrLn "hi!"
  tests >>= defaultMain

