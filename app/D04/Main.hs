module D04.Main where

import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Data.Universe.Helpers

xmasFile :: GenParser Char st [[XMAS]]
xmasFile = sepEndBy xmasLine endOfLine

xmasLine :: GenParser Char st [XMAS]
xmasLine = catMaybes <$> many (Just <$> parseXMAS <|> Nothing <$ (letter <|> char '.'))

data XMAS = XMAS deriving (Show)

parseXMAS :: GenParser Char st XMAS
parseXMAS = XMAS <$ string' "XMAS"

main :: IO ()
main = do
  base <- readFile "app/D04/input"
  let vert = unlines . transpose . lines
  let inv = unlines . (fmap reverse) . lines
  let diag = unlines . diagonals . lines
  let par = parse xmasFile ""
  let appAll = \f -> \x -> [x,f x]

  let a = traverse par [base, inv base, vert base, inv $ vert base, diag base, inv $ diag base,diag $ vert $ base, inv $ diag $ vert base]
  print $ a
  print $ length $ concat $ concat $ concat a


