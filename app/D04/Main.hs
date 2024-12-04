module D04.Main (main) where

import Data.Functor
import Data.List
import Data.Maybe
import Data.Universe.Helpers
import GHC.Arr (Ix (range), array)
import Text.Parsec
import Text.Parsec.String

xmasFile :: GenParser Char st [[XMAS]]
xmasFile = sepEndBy xmasLine endOfLine

xmasLine :: GenParser Char st [XMAS]
xmasLine = catMaybes <$> many (Just <$> parseXMAS <|> Nothing <$ (letter <|> char '.'))

data XMAS = XMAS deriving (Show)

parseXMAS :: GenParser Char st XMAS
parseXMAS = XMAS <$ string' "XMAS"

step1 :: String -> IO ()
step1 base = do
  let vert = unlines . transpose . lines
  let inv = unlines . (fmap reverse) . lines
  let diag = unlines . diagonals . lines
  let par = parse xmasFile ""

  let straights = [base, inv base, vert base, inv $ vert base]
  let diags = [diag base, inv $ diag base, diag $ inv $ base, inv $ diag $ inv $ base]
  let out = traverse par (straights ++ diags)
  print $ length $ concat $ concat $ concat out

data PossCross = PossCross {center :: Char, tl :: Char, tr :: Char, bl :: Char, br :: Char} deriving (Show, Eq)

isCrossMass :: PossCross -> Bool
isCrossMass PossCross {center = 'A', tl = 'M', tr = 'M', bl = 'S', br = 'S'} = True
isCrossMass PossCross {center = 'A', tl = 'S', tr = 'M', bl = 'S', br = 'M'} = True
isCrossMass PossCross {center = 'A', tl = 'M', tr = 'S', bl = 'M', br = 'S'} = True
isCrossMass PossCross {center = 'A', tl = 'S', tr = 'S', bl = 'M', br = 'M'} = True
isCrossMass _ = False

step2 :: String -> IO ()
step2 base = do
  let tableau = lines base :: [[Char]]
  let gElem = \(x, y) -> tableau !! y !! x
  print "Step 2"
  let maxx = length (tableau !! 0) - 1
  let maxy = length tableau - 1
  let coords_list = range ((0, 0), (maxx, maxy))
  let mapped =
        catMaybes $
          coords_list <&> \(x, y) ->
            if x == 0 || y == 0 || x == maxx || y == maxy
              then
                Nothing
              else
                Just $ PossCross {center = gElem (x, y), tl = gElem (x - 1, y - 1), tr = gElem (x + 1, y - 1), bl = gElem (x - 1, y + 1), br = gElem (x + 1, y + 1)}
  print $ length $ filter id $ isCrossMass <$> mapped

main :: IO ()
main = do
  base <- readFile "app/D04/input"
  step1 base
  step2 base
