{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module D09.Main (main) where

import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace (trace)
import Text.Parsec (digit, endOfLine, many1, optionMaybe, parse)
import Text.Parsec.String (GenParser)
import Text.Printf (printf)

type FileID = Int

type FileSize = Int

blockParser :: GenParser Char st (Int -> [Maybe Int])
blockParser = do
  len <- read <$> return <$> digit
  emptyD <- optionMaybe digit
  let emptyList = case emptyD of
        Just e -> replicate (read [e]) Nothing
        Nothing -> []
  return $ (\x -> (replicate len (Just x)) ++ emptyList)

fileParser :: GenParser Char st [Maybe FileID]
fileParser = concat <$> fmap (\(i, f) -> f i) <$> zip [0 ..] <$> many1 blockParser <* endOfLine

fileParser2 :: GenParser Char st [((FileID, FileSize), FileSize)]
fileParser2 = fmap (\(i, (s, f)) -> ((i, s), f)) <$> zip [0 ..] <$> many1 ((,) <$> (read <$> return <$> digit) <*> (fromMaybe 0 <$> optionMaybe (read <$> return <$> digit))) <* endOfLine

defrag :: [Maybe Int] -> [Int]
defrag ls =
  let recdefrag :: [Maybe Int] -> [Maybe Int] -> [Int] -> [Int]
      recdefrag (Nothing : xs) (Just o : other) cur = recdefrag xs other (o : cur)
      recdefrag xs@(Nothing : _) (Nothing : other) cur = recdefrag xs other cur
      recdefrag (Just i : xs) other cur = recdefrag xs other (i : cur)
      recdefrag (Nothing : xs) other cur = recdefrag xs other cur
      recdefrag [] [] cur = cur
      recdefrag [] (Just i : xs) cur = recdefrag [] xs (i : cur)
      recdefrag [] (Nothing : xs) cur = recdefrag [] xs cur
   in take (length $ catMaybes ls) $ reverse $ recdefrag (ls) (reverse ls) []

type Disk = [((FileID, FileSize), FileSize)]

--defrag2 :: Disk -> Disk
defrag2 ls =
  let recdefrag :: Disk -> Disk -> [FileID] -> (Disk,[FileID])
      recdefrag (((lid, lsize), lfree) : lxs) (((rid, rsize), rfree) : rxs) unmoved | rsize <= lfree = recdefrag (((lid, lsize), 0) : ((rid, rsize), lfree - rsize) : lxs) rxs unmoved
      recdefrag ldisk@(((lid, lsize), lfree) : lxs) (((rid, rsize), rfree) : rxs) unmoved = recdefrag ldisk rxs (rid : unmoved)
      recdefrag ldisk [] unmoved = (ldisk,unmoved)
   in recdefrag ls (reverse ls) []

main :: IO ()
main = do
  putStrLn "Day 9"
  file <- readFile "app/D09/input"
  let dat = parse fileParser2 "" file
  case dat of
    Left err -> do
      print "erreur de parsing"
      print err
    Right val ->
      do
        --        let defragged = defrag val
        --        let r = sum $ (uncurry ($)) <$> zip [(* i) | i <- [0 ..]] defragged
        print $ defrag2 val
