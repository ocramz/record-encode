module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [
  "src/Data/Record/Encode.hs"
  ]
