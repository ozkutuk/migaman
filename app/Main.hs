module Main where

import qualified MyLib (someFunc)
import qualified Migadu

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
