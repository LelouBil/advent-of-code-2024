{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Utils.Data
  ( VerticalDirection,
    HorizontalDirection,
    CardinalDirection,
    sndM,
    fstM,
    Point,
    Size,
    Map2D,
    mapFromList,
    CharMapper (..),
    parse2DMap,
    show2DMap,
    mapToStore,
    mapFromStore,
    matchMap,
    storeMapNeighborhood,
  )
where

import Control.Comonad.Store (ComonadStore (experiment, peeks), Store, store)
import Control.Lens (Field1 (_1), maximum1Of, maximumOf, traverse1Of_, view)
import Data.Bifunctor (Bifunctor (second))
import Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (All (All, getAll))
import Data.Set (Set)
import Data.Tuple (swap)
import Debug.Trace (trace)
import Linear (V2 (..))
import qualified Linear.V2 as L
import Text.Parsec (anyChar, endOfLine, lookAhead, many1, manyTill, sepEndBy1)
import Text.Parsec.String (GenParser)
import Text.Printf (printf)

type Point = V2 Int

type Size = V2 Int

type Map2D a = M.Map Point a

data VerticalDirection = UpDir | DownDir

data HorizontalDirection = LeftDir | RightDir

data CardinalDirection = North | East | South | West

instance Direction CardinalDirection where
  toCardinal = id
  dir = \case
    V2 0 1 -> Just North
    V2 0 (-1) -> Just South
    V2 1 0 -> Just East
    V2 (-1) 0 -> Just West
    _ -> Nothing

class Direction a where
  toCardinal :: a -> CardinalDirection
  dir :: (V2 Int) -> Maybe a

instance Direction VerticalDirection where
  toCardinal = \case
    UpDir -> North
    DownDir -> South
  dir = \case
    V2 0 1 -> Just UpDir
    V2 0 (-1) -> Just DownDir
    _ -> Nothing

instance Direction HorizontalDirection where
  toCardinal = \case
    LeftDir -> East
    RightDir -> West
  dir = \case
    V2 1 0 -> Just RightDir
    V2 (-1) 0 -> Just LeftDir
    _ -> Nothing

type ReadMapperFn a = Char -> Maybe a

type ShowMapperFn a = a -> Char

data CharMapper a = CharMapper {readMapper :: ReadMapperFn a, showMapper :: ShowMapperFn a}

class ShowMappingProducer a b | a -> b where
  toShowMapper :: a -> ShowMapperFn b

class ReadMappingProducer a b | a -> b where
  toReadMapper :: a -> ReadMapperFn b

instance ReadMappingProducer (ReadMapperFn a) a where
  toReadMapper = id

instance ReadMappingProducer (CharMapper a) a where
  toReadMapper = readMapper

instance ShowMappingProducer (CharMapper a) a where
  toShowMapper = showMapper

instance ShowMappingProducer (ShowMapperFn a) a where
  toShowMapper = id

mapFromList :: (Eq a) => [(Char, a)] -> CharMapper a
mapFromList xs = CharMapper (flip lookup xs) (fromMaybe '?' . (flip lookup $ swap <$> xs))

parse2DMap :: (ReadMappingProducer a b) => a -> GenParser Char st (Size, M.Map Point b)
parse2DMap charMapper = do
  rows <- concatMap (\(y, xs) -> fmap (\(x, v) -> (V2 x y, v)) xs) <$> (zip [0 ..] <$> sepEndBy1 (zip [0 ..] <$> manyTill (anyChar) (lookAhead endOfLine)) endOfLine)
  let width = fromMaybe 0 $ (+ 1) <$> maximumOf (traverse . _1 . L._x) rows
  let height = fromMaybe 0 $ (+ 1) <$> maximumOf (traverse . _1 . L._y) rows
  return $ (V2 width height,) $ M.fromList $ catMaybes (sndM <$> second (toReadMapper charMapper) <$> rows)

show2DMap :: (ShowMappingProducer a b) => a -> (Size, M.Map Point b) -> String
show2DMap mapper (V2 width height, m) = unlines [[toShowMapper mapper $ (m ! V2 x y) | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]

sndM :: (a, Maybe b) -> Maybe (a, b)
sndM (a, Just b) = Just (a, b)
sndM (_, Nothing) = Nothing

fstM :: (Maybe a, b) -> Maybe (a, b)
fstM (Just a, b) = Just (a, b)
fstM (Nothing, _) = Nothing

mapToStore :: (Ord k, Num k) => M.Map k a -> Store k (Maybe a)
mapToStore mp = store (`M.lookup` mp) 0

mapFromStore :: (Num k) => Set k -> Store k a -> M.Map k a
mapFromStore ks = experiment \x -> M.fromSet (+ x) ks

-- checkStencil :: (Num k, Eq a) => M.Map k a -> Store k (Maybe a) -> Bool
-- checkStencil mp x = all (\(p, expected) -> peeks (+ p) x == Just expected) (M.toList mp)
-- checkStencil == matchMap, but using the All Monoid instead of a list and the all function
matchMap :: (Num k, Eq a) => M.Map k a -> Store k (Maybe a) -> Bool
matchMap mp = getAll . storeMapNeighborhood (fmap All . (==) . Just <$> mp)

storeMapNeighborhood :: (Num k, Monoid b) => M.Map k (a -> b) -> Store k a -> b
storeMapNeighborhood mp x = M.foldMapWithKey (\p f -> f $ peeks (+ p) x) mp
