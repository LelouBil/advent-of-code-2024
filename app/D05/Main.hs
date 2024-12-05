{-# LANGUAGE TupleSections #-}

module D05.Main (main) where

import Control.Arrow
import Control.Monad (join)
import D01.Main (toPair)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Universe.Helpers
import GHC.Arr (Ix (range), array)
import Text.Parsec
import Text.Parsec.String
import Utils.ParseLib (integer)

data OrderRule = OrderRule Int Int deriving (Show, Eq)

before, after :: OrderRule -> Int
before (OrderRule f l) = f
after (OrderRule f l) = l

pair :: OrderRule -> (Int, Int)
pair (OrderRule f l) = (f, l)

type PageList = [Int]

type Input = ([OrderRule], [PageList])

fileParser :: GenParser Char s Input
fileParser =
  (,)
    <$> (OrderRule <$> integer <* char '|' <*> integer) `sepEndBy1` endOfLine
    <*> (newline *> (integer `sepBy1` char ',') `sepEndBy1` endOfLine)

getMid :: [Int] -> Int
getMid l = l !! (length l `div` 2)

getBeforeFor :: [OrderRule] -> Int -> [Int]
getBeforeFor rules e = before <$> filter ((e ==) . after) rules

isElemCorrect :: [Int] -> [Int] -> Bool
isElemCorrect wantedBefore bef = (intersect wantedBefore bef) == wantedBefore

isCorrect :: [OrderRule] -> PageList -> Bool
isCorrect r list =
  let isElemCorrect' = \i e -> isElemCorrect (intersect list $ getBeforeFor r e) (take (i + 1) list)
   in all
        (uncurry isElemCorrect')
        $ zip [0 ..] list

step1 :: Input -> IO ()
step1 (rules, inputs) = do
  print . sum . fmap getMid . (filter (isCorrect rules)) $ inputs

reconstruct :: [OrderRule] -> [Int]
reconstruct [] = []
reconstruct [OrderRule b a] = [b, a]
reconstruct rules =
  let allBefore = before <$> rules
      allAfter = after <$> rules
      allI = union allBefore allAfter
      curF = head (filter (`notElem` allAfter) allI)
   in curF : reconstruct (filter (\o -> (before o) /= curF) rules)

-- easy, not fun, not recursive method
-- rulesCompare :: [OrderRule] -> Int -> Int -> Ordering
-- rulesCompare rules = \a b -> if OrderRule a b `elem` rules then LT else if OrderRule b a `elem` rules then GT else EQ

step2 :: Input -> IO ()
step2 (rules, inputs) = do
  let filterRules = \i -> filter (uncurry (&&) . join bimap (`elem` i) . pair) (rules)
  let fixed = reconstruct . filterRules <$> filter (not . isCorrect rules) inputs
  --  let fixed = sortBy (rulesCompare rules) <$> filter (not . isCorrect rules) inputs
--  _ <- traverse print fixed
  print $ sum $ getMid <$> fixed
  return ()

main :: IO ()
main = do
  base <- parse fileParser "" <$> readFile "app/D05/input"
  case base of
    Right p -> step2 p
    Left e -> print e

--  step2 base
