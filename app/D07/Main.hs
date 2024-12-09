{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module D07.Main (main) where

import Control.Applicative
import Control.Lens (sumOf)
import Control.Monad
import Control.Monad (guard, liftM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), hoistMaybe)
import Control.Monad.Writer.Lazy (MonadTrans (lift), MonadWriter (tell, writer), Writer, runWriter)
import Data.Bifunctor (Bifunctor (second))
import Data.Functor
import Data.List (intercalate)
import Data.Tree (unfoldTree)
import Text.Parsec (State, char, endOfLine, many1, parse, sepBy1, sepEndBy1, string')
import Text.Parsec.String (GenParser)
import Text.Printf (printf)
import Utils.ParseLib (integer)

fileParser :: GenParser Char st [(Integer, [Integer])]
fileParser = sepEndBy1 ((,) <$> integer <*> (string' ": " *> sepBy1 integer (many1 $ char ' '))) endOfLine

guarded :: (a -> Bool) -> a -> Maybe a
guarded cond i = if cond i then Just i else Nothing

cat :: Integer -> Integer -> Integer
cat a b = read (show a ++ show b)

choiceComputation :: Integer -> [Integer] -> MaybeT (Writer [String]) Integer
choiceComputation target (a : h : b) = MaybeT $ do
  let next f name = do
        let val = a `f` h
        guard (val <= target)
        lift $ tell name
        choiceComputation target (val : b)
  let equalsTarget = hoistMaybe . guarded (== target)
  let m = next (*) ["mul"] >>= equalsTarget
  let p = next (+) ["plus"] >>= equalsTarget
  let c = next cat ["cat"] >>= equalsTarget
  runMaybeT $ m <|> p <|> c
choiceComputation _ (a : _) = MaybeT $ writer (Just a, [])
choiceComputation _ _ = error ""

main :: IO ()
main = do
  putStrLn "Day 7"
  dat <- parse fileParser "" <$> readFile "app/D07/input"
  case dat of
    Left err -> print err
    Right val -> do
      putStrLn " --- Step 1 --- "
      let a = second runWriter . second runMaybeT . (\(x, y) -> (x, choiceComputation x y)) <$> val
      --      _ <-
      --        sequence $
      --          a <&> \case
      --            (i, (Just _, steps)) -> putStrLn $ printf "Yes : %d : %s" i (intercalate " " steps)
      --            (i, (Nothing, _)) -> putStrLn $ printf "No %d" i
      let step1answer =
            sum $
              ( \case
                  (_, (Just v, _)) -> v
                  _ -> 0
              )
                <$> a
      putStrLn $ printf "Answer : %d" step1answer
