module Main where

import Criterion.Main
import D12.Main (day)
import Utils.AOC (runDay, benchDay)


main :: IO ()
main = do
  let today = day
  putStrLn "Running"
  runDay today
  putStrLn "Benching"
  defaultMain [bench "day" $  benchDay today]
