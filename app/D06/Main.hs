{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module D06.Main (main) where

import Control.Lens
import Control.Monad.State (MonadState (get, put), State, evalState, execState, runState)
import Data.Bifunctor (Bifunctor (second))
import Data.List (elemIndex, find, findIndex, uncons, nub)
import Data.List.Lens
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Universe.Helpers
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf, genericRnfV1)
import GHC.Generics
import Prelude hiding (Left, Right)

data MapCellType = Obstacle | Clear deriving (Eq,Generic)
instance NFData MapCellType

data GuardOrientation = Up | Left | Down | Right deriving (Eq,Generic)
instance NFData GuardOrientation

data MapCell = MapCell {_typ :: MapCellType, _guard :: Maybe GuardOrientation, _walked :: Bool, _possibleObstacle :: Bool} deriving (Generic)

$(makeLenses ''MapCell)

instance NFData MapCell

newtype WorldMap = WorldMap [[MapCell]]
--instance NFData WorldMap where rnf = genericRnf

instance Show GuardOrientation where
  show Up = "^"
  show Left = "<"
  show Down = "v"
  show Right = ">"

instance Show WorldMap where
  show (WorldMap mapArr) = unlines $ fmap (concat . fmap show) mapArr

instance Show MapCellType where
  show Obstacle = "#"
  show Clear = "."

instance Show MapCell where
  show MapCell {_typ = Clear, _guard = Just x} = show x
  show MapCell {_walked = True, _typ = Clear, _guard = Nothing} = "x"
  show MapCell {_typ = Obstacle, _guard = Nothing, _walked = False} = "#"
  show MapCell {_typ = Clear, _guard = Nothing, _walked = False, _possibleObstacle = False} = "."
  show MapCell {_typ = Clear, _guard = Nothing, _walked = False, _possibleObstacle = True} = "O"
  show _ = "?"

charToCell :: Char -> MapCell
charToCell '#' = MapCell {_typ = Obstacle, _guard = Nothing, _walked = False, _possibleObstacle = False}
charToCell '.' = MapCell {_typ = Clear, _guard = Nothing, _walked = False, _possibleObstacle = False}
charToCell '^' = MapCell {_typ = Clear, _guard = Just Up, _walked = False, _possibleObstacle = False}
charToCell '>' = MapCell {_typ = Clear, _guard = Just Right, _walked = False, _possibleObstacle = False}
charToCell 'v' = MapCell {_typ = Clear, _guard = Just Down, _walked = False, _possibleObstacle = False}
charToCell '<' = MapCell {_typ = Clear, _guard = Just Left, _walked = False, _possibleObstacle = False}
charToCell c = error $ "Impossible character : " ++ [c]

parseWorld :: String -> WorldMap
parseWorld = WorldMap . fmap (fmap charToCell) . lines

guardInfo :: WorldMap -> Maybe ((Int, Int), GuardOrientation)
guardInfo (WorldMap m) = do
  let a = filter (isJust . snd) $ map (second $ findIndex $ isJust . _guard) (zip [0 ..] m)
  ((gi, Just gd), _) <- Data.List.uncons a
  let guardPos = (gi, gd)
  (Just guardDir) <- m ^? (ix (fst guardPos) . ix (snd guardPos) . guard)
  return (guardPos, guardDir)

walkDir :: GuardOrientation -> (Int, Int)
walkDir orient =
  case orient of
    Up -> (0, -1)
    Left -> (-1, 0)
    Right -> (1, 0)
    Down -> (0, 1)

rotateRight :: GuardOrientation -> GuardOrientation
rotateRight orient =
  case orient of
    Up -> Right
    Right -> Down
    Down -> Left
    Left -> Up


data StepResult = Walked | Turned | Exited deriving (Show, Eq)

squareFrontOfGuard :: WorldMap -> Maybe MapCell
squareFrontOfGuard (WorldMap m) = do
  ((gy, gx), orient) <- guardInfo $ WorldMap m
  let (dx, dy) = walkDir orient
  m ^? (ix (gy + dy) . ix (gx + dx))

guardStep :: State WorldMap StepResult
guardStep = do
  WorldMap m <- get
  let Just ((gy, gx), orient) = guardInfo $ WorldMap m
  let (dx, dy) = walkDir orient
  let lookingPos = (ix (gy + dy) . ix (gx + dx))
  let guardPos = (ix gy . ix gx)
  let withWalked = m & (guardPos . walked .~ True)
  let withoutGuard = withWalked & (guardPos . guard .~ Nothing)
  case _typ <$> squareFrontOfGuard (WorldMap m) of
    Nothing -> do
      put $ WorldMap withoutGuard
      return Exited
    Just Clear -> do
      put $ WorldMap $ (withoutGuard & (lookingPos . guard .~ (Just orient)))
      return Walked
    Just Obstacle -> do
      put $ WorldMap $ (withWalked & ((guardPos . guard . mapped) `over` rotateRight))
      return Turned

runUntil :: State a b -> (b -> Bool) -> a -> a
runUntil stateF checkF st = case runState stateF st of
  (ot, ns) | not $ checkF ot -> runUntil stateF checkF ns
  (_, ns) -> ns

runUntil' :: State WorldMap StepResult -> (StepHist -> WorldMap -> StepResult -> Bool) -> WorldMap -> StepHist -> (StepResult, WorldMap, StepHist)
runUntil' stateF checkF st hst =
  let gif = guardInfo st
   in case (gif, runState stateF st) of
        (Just gi, (ot, ns)) | not $ checkF hst ns ot -> runUntil' stateF checkF ns (gi : hst)
        (Just gi, (ot, ns)) -> (ot,ns,gi : hst)
        (_, (ot, ns)) -> (ot, ns, hst)

type StepHist = [((Int, Int), GuardOrientation)]



hasLooped :: StepHist -> WorldMap -> StepResult -> Bool
hasLooped h wm r =
  fromMaybe
    False
    ( do
        i <- guardInfo wm
        return $ i `elem` h
    )


doesLoop :: WorldMap -> Bool
doesLoop wp = let (res, _, _) = (runUntil' guardStep (\a (WorldMap b) c -> hasLooped a (WorldMap b) c || c == Exited) wp (catMaybes [guardInfo wp])) in res /= Exited

main :: IO ()
main = do
  dat <- readFile "app/D06/input"
  print "Day 6"
  let mp = parseWorld dat
  let WorldMap ml = mp
--  putStrLn $ show mp
--  print $ guardInfo mp
  let (_,_,fulhist) = runUntil' guardStep (\a b c -> c == Exited) mp $ catMaybes [guardInfo mp]
  print "Finished run 1"
--  print $ reverse $ take ( length fulhist - 2) fulhist
  let replaceLens = \(y, x) -> (ix y) . (ix x) . typ .~ Obstacle
  let withObstacles = map (\pos -> (replaceLens pos ml)) $  (reverse $ nub $ fst <$> fulhist)
  let withDoesLoop = ( (doesLoop . WorldMap <$> withObstacles) `using` parList rdeepseq)
  print $ "Number to test : "
  print $ length withDoesLoop
  print $ length $ filter id withDoesLoop

