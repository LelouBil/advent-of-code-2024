{-# LANGUAGE BlockArguments #-}

module D11.Main (day) where

import Control.Monad (void)
import Data.Function (fix)
import Data.Function.Memoize (memoize2)
import qualified Data.Map as M
import Data.MemoTrie
import qualified Data.Tree as T
import Text.Parsec (char, endOfLine, many1, sepEndBy1, (<|>))
import Text.Parsec.String (GenParser)
import Utils.AOC (mkSolution, mkSolutionT, AOCDay)
import Utils.ParseLib (integer, sepBySpaces1)

parseRocks :: GenParser Char st Rocks
parseRocks = sepBySpaces1 integer

type Rocks = [Int]

toDigits :: Int -> [Int]
toDigits n
  | n <= 0 = []
  | otherwise = numToDigits (n `mod` 10) (n `div` 10) []
  where
    numToDigits a 0 l = (a : l)
    numToDigits a b l = numToDigits (b `mod` 10) (b `div` 10) (a : l)

fromDigits :: [Int] -> Int
fromDigits = foldl (\acc x -> acc * 10 + x) 0

blink :: Int -> Rocks
blink r = do
  let rstr = toDigits r
  let rstrLength = length rstr
  case r of
    0 -> [1]
    _
      | (rstrLength `mod` 2 == 0) -> fromDigits <$> ((\(a, b) -> [a, b]) $ splitAt (rstrLength `div` 2) rstr)
      | otherwise -> [r * 2024]

blinkCountLen :: (Int -> Int -> Int) -> Int -> Int -> Int
blinkCountLen f 0 i = 1
blinkCountLen f n i = sum $ f (n - 1) <$> blink i

blinkCountLenMemo :: Int -> Int -> Int
blinkCountLenMemo = fix (memoize2 . blinkCountLen)

step1 :: Rocks -> IO Int
step1 rs = do
  let asTr e = T.unfoldTree (\r -> (r, blink r)) e
  let treeList = asTr <$> rs
  let lvs = (\t -> (T.levels t) !! 25) <$> treeList
  return $ sum $ (blinkCountLenMemo 25 <$> rs)

step2 :: Rocks -> IO Int
step2 rs = return $ sum $ (blinkCountLenMemo 75 <$> rs)

day :: AOCDay Int Int st Rocks stb Rocks
day = mkSolution 11 (parseRocks, step1) (parseRocks, step2)
