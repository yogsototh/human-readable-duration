module Main where

import Criterion.Main
import Data.Duration

main :: IO ()
main = defaultMain
  [ bgroup "to human"
    [ bench "Basic" $ nf humanReadableDuration 120200303 ]]
