module Utils.AOC (AOCDay, mkSolution, mkSolutionT, runDay, benchDay, idParse) where

import Control.Exception
import System.IO.Silently
import Control.Monad.Except (liftEither)
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Trans.Except
import Criterion.Main (Benchmarkable, nfIO, whnfIO)
import Data.Bifunctor (Bifunctor (first, second))
import System.Directory
import System.IO (readFile')
import Text.Parsec (SourceName, anyChar, many, parse)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (GenParser)
import Text.Printf (printf)

data AOCDay a b st i stb ii = AOCDay {num :: Int, step1 :: (DayParser st i, i -> IO a), step2 :: (DayParser stb ii, ii -> IO b), real :: Bool}

data DayParser st i = DayParser (GenParser Char st i)

idParse :: GenParser Char st String
idParse = many anyChar

mkSol :: (Show a) => (Show b) => Int -> (GenParser Char st i, i -> IO a) -> (GenParser Char stb ii, ii -> IO b) -> Bool -> AOCDay a b st i stb ii
mkSol a (b, c) (d, e) = AOCDay a (DayParser b, c) (DayParser d, e)

mkSolution, mkSolutionT :: (Show a) => (Show b) => Int -> (GenParser Char st i, i -> IO a) -> (GenParser Char stb ii, ii -> IO b) -> AOCDay a b st i stb ii
mkSolution a b c = mkSol a b c True
mkSolutionT a b c = mkSol a b c False

data AOCException = ParseExcept ParseError | IOExcept IOError deriving (Show)

runWithInput :: (DayParser () i, i -> IO a) -> SourceName -> ExceptT AOCException IO a
runWithInput (DayParser parser, runner) filepath = do
  unparsedResult <- liftIO $ try (readFile filepath)
  unparsed <- liftEither $ first IOExcept unparsedResult
  input <- liftEither $ first ParseExcept $ parse parser filepath unparsed
  result <- lift $ runner input
  lift $ return result

runDayInput :: (Show a) => (Show b) => AOCDay a b () i () ii -> String -> ExceptT AOCException IO ()
runDayInput day filePath = do
  liftIO $ putStrLn "Step 1"
  a <- runWithInput (step1 day) filePath
  liftIO $ putStrLn $ printf "Result : %s" (show a)
  liftIO $ putStrLn "Step 2"
  b <- runWithInput (step2 day) filePath
  liftIO $ putStrLn $ printf "Result : %s" (show b)
  return ()

-- only run real
benchDay :: (Show a) => (Show b) => AOCDay a b () i () ii -> Benchmarkable
benchDay day = whnfIO $ silence $ do
  putStrLn $ printf "BENCH -- Day %d" (num day)
  putStrLn "Real File"
  real_res <- runExceptT $ runDayInput day (printf "app/D%02d/input" (num day))
  case real_res of
    Left e -> putStrLn $ printf "Error : %s" (show e)
    Right _ -> return ()

runDay :: (Show a) => (Show b) => AOCDay a b () i () ii -> IO ()
runDay day = do
  putStrLn $ printf "Day %d" (num day)
  putStrLn "-> Test File"
  test_res <- runExceptT $ runDayInput day (printf "app/D%02d/input_test" (num day))
  case test_res of
    Left e -> putStrLn $ printf "Error : %s" (show e)
    Right _ -> return ()
  if real day
    then do
      putStrLn "-> Real File"
      real_res <- runExceptT $ runDayInput day (printf "app/D%02d/input" (num day))
      case real_res of
        Left e -> putStrLn $ printf "Error : %s" (show e)
        Right _ -> return ()
    else return ()
