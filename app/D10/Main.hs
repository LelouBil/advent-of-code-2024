{-# LANGUAGE BlockArguments #-}

module D10.Main (day) where

import Control.Comonad.Store
import qualified Data.Foldable as M
import Data.List (nub)
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (All (All, getAll))
import Data.Set (Set)
import Data.Tree (Tree, drawTree, foldTree, unfoldTree, unfoldTreeM)
import Debug.Trace (trace)
import Linear (V2 (V2))
import Text.Parsec (char)
import Text.Printf (printf)
import Utils.AOC (mkSolution, mkSolutionT)
import Utils.Data (CharMapper (..), Map2D, Point, ensureMaybe, mapFromStore, mapToStore, matchMap, parse2DMap, show2DMap)

charMapper :: CharMapper Int
charMapper =
  CharMapper
    ( \case
        '.' -> Nothing
        c -> Just $ read $ pure c
    )
    ( \case
        Nothing -> '.'
        Just c -> head $ show c
    )


hikingTree :: Point -> Store Point (Maybe Int) -> Tree Point
hikingTree start maps =
  let neighbours = [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1)]
      hike pt =
        let pval = peek pt maps
         in case pval of
              Nothing -> (pt, [])
              Just p ->
                ( pt,
                  filter
                    ( \t ->
                        fromMaybe False $ fmap (\v -> v - p == 1) (peek t maps)
                    )
                    $ (+ (pt))
                      <$> neighbours
                )
   in unfoldTree hike start

trailHeadScore :: Store Point (Maybe Int) -> Int
trailHeadScore maps = length $ nub $ last <$> (paths (hikingTree (pos maps) maps))

paths :: Tree Point -> [[Point]]
paths tr =
  let folder :: Point -> [[[Point]]] -> [[Point]]
      folder a [] = [[a]]
      folder a sub = concat $ fmap (fmap (a :)) sub
   in filter (\x -> length x == 10) $ foldTree folder tr

step1 :: (Point, Map2D Int) -> IO Int
step1 (size, m) = do
  print size
  let st = mapToStore m
  --  print $ hikingTree (V2 3 0) st
  let hikeTree = extend trailHeadScore $ st
  let ts = mapFromStore (M.keysSet m) hikeTree
  --  putStrLn $ show2DMap charMapper (size, ts)
  let trailSum = M.sum ts
  return trailSum

step2 :: (Point, Map2D Int) -> IO Int
step2 (size, m) = do
  let st = mapToStore m
  let hikeTree = extend (\s -> length $ paths $ hikingTree (pos s) s) $ st
  let ts = mapFromStore (M.keysSet m) hikeTree
--  print ts
  return $ M.sum ts

day = mkSolution 10 (parse2DMap charMapper, step1) (parse2DMap charMapper, step2)
