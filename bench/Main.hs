module Main where

import Criterion.Main
import Data.Duration

main :: IO ()
main = defaultMain
  [ bgroup "to human"
    [ bench "small" $ nf humanReadableDuration 0.123456
    , bench "Basic" $ nf humanReadableDuration 987654321.123456 ]]
