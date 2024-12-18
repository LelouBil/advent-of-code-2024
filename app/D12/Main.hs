module D12.Main (day) where

import Control.Comonad.Store (Comonad (extend), ComonadStore (peek, peeks), Store)
import Control.Monad (void)
import Data.Function (fix)
import Data.Function.Memoize (memoize2)
import Data.List (nub, uncons)
import qualified Data.Map as M
import Data.Map.Lazy (keysSet)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.MemoTrie
import qualified Data.Tree as T
import Debug.Trace
import GHC.Base (VecElem (Int16ElemRep))
import Linear (V2 (V2))
import Text.Parsec (char, endOfLine, many1, sepEndBy1, (<|>))
import Text.Parsec.String (GenParser)
import Text.Printf (printf)
import Text.Read (Lexeme (String))
import Utils.AOC (AOCDay, mkSolution, mkSolutionT)
import Utils.Data (CharMapper (CharMapper), Map2D, Point, Size, ensureMaybe, mapFromStore, mapToStore, parse2DMap, show2DMap)
import Utils.ParseLib (integer, sepBySpaces1)
import Control.Monad.State (State, withState, gets, MonadState (get, put))
import qualified Data.Map.Merge.Lazy as M
import Data.Map.Merge.Lazy (preserveMissing, zipWithMatched)

type Plant = Char

type Garden = Map2D Plant

charMapper :: CharMapper Plant
charMapper = CharMapper Just (fromMaybe '.')

neighbours :: [V2 Int]
neighbours = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

perimeterValue :: Store Point (Maybe Plant) -> Int
perimeterValue st =
  let currentPlant = fromMaybe '.' $ peeks id st
      perimValue = sum $ fromMaybe 1 <$> fmap (\np -> if np /= currentPlant then 1 else 0) <$> (flip peeks st . (+)) <$> neighbours
   in perimValue

findRegionInfo :: Garden -> [Point] -> Point -> [Point]
findRegionInfo garden area start
  | isNothing $ start `M.lookup` garden = []
  | otherwise =
      --      trace (printf "findRegionInfo %s" (show start)) $
      nub $
        area
          ++ ( concat $
                 catMaybes $
                   ( fmap
                       (findRegionInfo garden (start : area))
                       . ensureMaybe (\n -> (start `M.lookup` garden) == (n `M.lookup` garden) && (n `notElem` area))
                       . Just
                       . (+ start)
                   )
                     <$> neighbours
             )

type RegionHeadMap = M.Map Point Point
findRegionHead :: Garden -> Point -> State RegionHeadMap (Maybe Point)
findRegionHead garden pt = do
    rhm <- get
    case pt `M.lookup` rhm of
        Nothing -> do
            let ri = uncons $ findRegionInfo garden [] pt
            case ri of
                Nothing -> return Nothing
                Just (h,xs) -> do
                    let nm = M.merge preserveMissing preserveMissing (zipWithMatched (\x -> \y -> (x,y))) rhm rhm
                    return $ Just h
        Just h -> return $ Just h

step1 :: (Size, Garden) -> IO Int
step1 (size, garden) = do
  let perimMap = mapFromStore (keysSet garden) $ (extend perimeterValue) (mapToStore garden)
  putStrLn $ show2DMap (fromMaybe '.' . fmap (head . show)) (size, perimMap)
  let oregion = findRegionInfo garden [] (V2 0 0)
  print oregion
  print $ length oregion
  print $ sum $ (\p -> fromMaybe 0 (p `M.lookup` perimMap)) <$> oregion
  return 0

step2 :: (Size, Garden) -> IO Int
step2 (size, garden) = do
  return 0

day :: AOCDay Int Int st (Size, Garden) stb (Size, Garden)
day = mkSolutionT 12 (parse2DMap charMapper, step1) (parse2DMap charMapper, step2)
