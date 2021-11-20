module Main where

import qualified MyLib (someFunc)

main :: IO ()
main =
  MyLib.someFunc
