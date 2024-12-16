{-# LANGUAGE BlockArguments #-}
module D10.Main (day) where

import Utils.AOC (mkSolutionT)
import Utils.Data (CharMapper (..), Map2D, Point, parse2DMap, show2DMap, matchMap, mapFromStore, mapToStore)
import qualified Data.Map.Lazy as M
import Text.Printf (printf)
import Debug.Trace (trace)
import Control.Comonad.Store
import Linear (V2(V2))
import Data.Set (Set)
import Data.Monoid (All(getAll, All))

charMapper :: CharMapper Int
charMapper = CharMapper (Just . read . pure) (head . show)

testStencil :: Map2D Int
testStencil = M.fromList [(V2 0 0,8),(V2 1 0,7)]



boolShow :: Bool -> Char
boolShow True = '#'
boolShow False = '.'

step1 :: (Point, Map2D Int) -> IO Int
step1 (size, m) = do
  print size
--  print m
  putStrLn $ show2DMap charMapper (size, m)
  let st = mapToStore m
  print $ experiment (\x -> [x]) st
  print $ matchMap m st
  let r = extend (matchMap testStencil) st
  -- turn r (StoreT Point Identity Bool) into a Map2D Bool
  let rm = mapFromStore (M.keysSet m) r
  putStrLn $ show2DMap boolShow (size, rm)

  return 0

day = mkSolutionT 10 (parse2DMap charMapper, step1) (parse2DMap charMapper, const $ return $ 0)
