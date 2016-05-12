module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "src/Data/Duration.hs"
               , "src/Data/Duration/Tutorial.hs"]
