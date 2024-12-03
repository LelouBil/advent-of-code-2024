module D01.Main where

import Data.Bifunctor
import Data.Functor
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Utils.ParseLib

data LineElem = LineElem Int Int
  deriving (Eq, Show)

toPair (LineElem l r) = (l, r)

lineFile :: GenParser Char st [LineElem]
lineFile = onePerLine line_elem

line_elem :: GenParser Char st LineElem
line_elem = LineElem <$> integer <*> (spaces *> integer)

diffPairs :: [(Int, Int)] -> [Int]
diffPairs = fmap $ abs . uncurry (-)

sortPairs :: [(Int, Int)] -> [(Int, Int)]
sortPairs = uncurry zip . bimap sort sort . Data.List.unzip

countO :: Eq e => e -> [e] -> Int
countO e = length . filter (e==)

countOccurencesInSec :: ([Int],[Int]) -> [(Int,Int)]
countOccurencesInSec (num,occlist) = fmap (\x -> (x,countO x occlist)) num

firstStar :: [LineElem] -> Int
firstStar = sum . diffPairs . sortPairs . fmap toPair

secondStar :: [LineElem] -> Int
secondStar = sum . fmap (uncurry (*)) . countOccurencesInSec . Data.List.unzip . fmap toPair

main = (parseFromFile lineFile "app/D01/input") <&> second secondStar >>= print
