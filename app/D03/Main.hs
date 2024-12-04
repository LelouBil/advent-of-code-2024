{-# LANGUAGE BlockArguments #-}

module D03.Main where

import Control.Monad.State
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Utils.ParseLib

data Mul = Mul Int Int deriving (Eq, Show)
data DoDont = DoDont Bool deriving (Eq, Show)
data Instruction = MulI Mul | DoDontI DoDont deriving (Eq, Show)

file :: GenParser Char st [Instruction]
file = catMaybes <$> many (try (Just <$> instruct) <|> (Nothing <$ anyChar))

instruct :: GenParser Char st Instruction
instruct = MulI <$> mulInstruct <|> DoDontI <$> doDontInstruct

mulInstruct :: GenParser Char st Mul
mulInstruct = Mul <$> (string' "mul(" *> integer) <*> (char ',' *> integer <* char ')')

doDontInstruct :: GenParser Char st DoDont
doDontInstruct = (DoDont False <$ string' "don't()") <|> (DoDont True <$ string' "do()")

machineStep :: Instruction -> Control.Monad.State.State Bool (Maybe Int)
machineStep (MulI (Mul l r)) = do
  isEnabled <- get
  return $ if isEnabled then Just (l * r) else Nothing
machineStep (DoDontI (DoDont isEnabled)) = put isEnabled >> return Nothing

main :: IO ()
main = do
  result <- parseFromFile file "app/D03/input"
  case result of
    Left err -> print err
    Right instructions -> print $ sum . catMaybes $ evalState (traverse machineStep instructions) True
