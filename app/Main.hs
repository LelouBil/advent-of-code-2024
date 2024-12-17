module Main where

import Criterion.Main
import D10.Main
import Utils.AOC (runDay, benchDay)


main :: IO ()
main = do
  let today = D10.Main.day
  putStrLn "Running"
  runDay today
  putStrLn "Benching"
  defaultMain [bench "day" $  benchDay today]
