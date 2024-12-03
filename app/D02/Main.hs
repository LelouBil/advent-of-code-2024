module D02.Main where

import Control.Arrow
import Data.Bifunctor
import Data.Functor
import Data.Ix
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Utils.ParseLib

file :: GenParser Char st [[Int]]
file = sepEndBy intLine endOfLine

intLine :: GenParser Char st [Int]
intLine = sepBy1 integer (many1 $ char ' ')

main :: IO ()
main = do
  Right e <- parseFromFile file "app/D02/input"
  print e
  print $ diffList <$> e
  print $ process e

process :: [[Int]] -> Int
process = length . filter (liftA2 (||) isSafeItself isSafeWithRemove)

isSafeItself :: [Int] -> Bool
isSafeItself =
  diffList >>> proc dl -> do
    allPositive <- all (> 0) -< dl
    allNegative <- all (< 0) -< dl
    withinRange <- all (abs >>> inRange (1, 3)) -< dl
    returnA -< (allPositive || allNegative) && withinRange

isSafeWithRemove :: [Int] -> Bool
isSafeWithRemove xs = ((`deleteNth` xs) >>> isSafeItself) `any` [0 .. length xs - 1]

deleteNth :: Int -> [a] -> [a]
deleteNth n xs = case splitAt n xs of
  (a, _ : b) -> a ++ b
  _ -> xs

diffList :: [Int] -> [Int]
diffList l = zipWith (-) l (drop 1 l)
