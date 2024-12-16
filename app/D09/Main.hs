module D09.Main (day) where

import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace (trace)
import Text.Parsec (digit, endOfLine, many1, optionMaybe, parse)
import Text.Parsec.String (GenParser)
import Text.Printf (printf)
import Utils.AOC (mkSolution, mkSolutionT)
import Utils.Data (sndM)

type FileID = Int

type FileSize = Int

type File = (FileID, FileSize)

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

removeFile :: FileID -> Disk -> Disk
removeFile targetid (((fid, fsize), spacebefore) : (prev_file, emptyspacebefore) : fxs) | fid == targetid = (prev_file, emptyspacebefore + fsize + spacebefore) : fxs
removeFile targetid (hf : fxs) = hf : removeFile targetid fxs
removeFile _ [] = []

defrag2 :: Disk -> Disk
defrag2 ls =
  let tryPlace :: Disk -> File -> Maybe Disk
      tryPlace [] _ = Nothing
      tryPlace ((prev_file, space_after) : fxs) new_file@(_, nf_size)
        | nf_size <= space_after = Just ((prev_file, 0) : (new_file, space_after - nf_size) : fxs)
      tryPlace (ff : fxs) trying_file = (ff :) <$> tryPlace fxs trying_file
      recdefrag :: Disk -> Disk -> Disk
      recdefrag main ((to_move, _) : tmxs) = recdefrag (fromMaybe main ((reverse . (removeFile $ fst to_move) . reverse) <$> tryPlace main to_move)) tmxs
      recdefrag m [] = m
   in recdefrag ls (reverse ls)

showDisk :: Disk -> String
showDisk [] = ""
showDisk (((fid, fsize), ffree) : fxs) = (concat $ replicate fsize (show fid)) ++ (replicate ffree '.') ++ (showDisk fxs)

checksum :: [Maybe Int] -> Int
checksum disk = sum $ (uncurry ($)) <$> catMaybes (sndM <$> zip [(* i) | i <- [0 ..]] disk)

expand :: Disk -> [Maybe Int]
expand (((fid, fsize), ffree) : fxs) = (replicate fsize (Just fid)) ++ replicate ffree Nothing ++ expand fxs
expand [] = []

step1 :: [Maybe Int] -> IO Int
step1 input = return $ checksum $ Just <$> defrag input

step2 :: Disk -> IO Int
step2 input = do
  print $ showDisk $ input
  let def = defrag2 input
  print $ showDisk def
  return $ checksum $ expand def

day = mkSolution 9 (fileParser, step1) (fileParser2, step2)
