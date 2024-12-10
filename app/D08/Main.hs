{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module D08.Main (main) where

import Control.Arrow
import Control.Lens (Bifunctor (bimap), Contains (contains), sumOf, view)
import Control.Monad
import Control.Monad (guard, liftM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), hoistMaybe)
import Control.Monad.Writer.Lazy (MonadTrans (lift), MonadWriter (tell, writer), Writer, runWriter)
import Data.Bifunctor (Bifunctor (second))
import Data.Function (on)
import Data.Functor
import Data.Ix (Ix (inRange))
import Data.List (groupBy, intercalate, intersect, maximumBy, nub, nubBy, sortOn)
import Data.Maybe (catMaybes)
import Data.Tree (unfoldTree)
import Data.Tuple (swap)
import Linear.V2
import Text.Parsec (State, alphaNum, char, endOfLine, many, many1, parse, sepBy1, sepEndBy1, string', (<|>))
import Text.Parsec.String (GenParser)
import Text.Printf (printf)
import Utils.ParseLib (integer)

type Point = V2 Int

type Antenna = (Point, Char)

lineParser :: GenParser Char st [(Int, Char)]
lineParser =
  catMaybes
    <$> ( map $ \case
            (i, Just c) -> Just (i, c)
            _ -> Nothing
        )
    <$> zip [0 ..]
    <$> many1 ((const Nothing <$> (char '.' <|> char '#')) <|> (Just <$> alphaNum))

fileParser :: GenParser Char st [Antenna]
fileParser =
  concatMap
    ( \(y, t) ->
        (\(x, c) -> (V2 x y, c)) <$> t
    )
    <$> zip [0 ..]
    <$> sepEndBy1 lineParser endOfLine

antinodes :: (Point, Point) -> [Antenna] -> [[(Point)]]
antinodes _ [] = []
antinodes _ [_] = []
antinodes maxrange xs =
  let samePair a b = a == b || a == swap b
      crosslist = nubBy samePair $ filter (uncurry (/=)) $ (,) <$> xs <*> xs
   in fmap
        ( \((l, _), (r, _)) ->
            let dist@(V2 distx disty) = l - r
                reducedDist = fmap (`div` abs (gcd distx disty)) dist
                (redop, oppop) = if view _x reducedDist < 0 || view _y reducedDist < 0 then ((+), (-)) else ((-), (+))
                lToBottom = takeWhile (inRange maxrange) [l `redop` (reducedDist * pure x) | x <- [0 ..]]
                lToInfinite = takeWhile (inRange maxrange) [l `oppop` (reducedDist * pure x) | x <- [1 ..]]
             in
                lToBottom ++ lToInfinite
        )
        crosslist

main :: IO ()
main = do
  putStrLn "Day 8"
  file <- readFile "app/D08/input"
  let dat = parse fileParser "" file
  let maxc = (length $ lines file, length $ (!! 0) . lines $ file)
  case dat of
    Left err -> do
      print "erreur de parsing"
      print err
    Right val ->
      do
        print maxc
        let a = groupBy ((==) `on` snd) $ sortOn snd val
        print a
        let (maxy, maxx) = maxc
        let inrange =
              nub $
                concat $
                  concat $
                    antinodes (pure 0, (V2 (maxx - 1) (maxy - 1)))
                      <$> a
        print inrange
        print $ length inrange
