module Utils.ParseLib(onePerLine, spaceChar, manySpaces, integer, sepBySpaces, sepBySpaces1) where
import Text.Parsec (optionMaybe, many, optional, option, newline, endOfLine, many1, digit, char, sepBy, sepBy1)
import Data.Maybe (maybeToList)
import Data.Functor
import Text.Parsec.String (GenParser)
integer :: GenParser Char st Int
integer = do
  d <- many1 digit
  return $ read d


spaceChar :: GenParser Char st ()
spaceChar = char ' ' *> return ()

manySpaces :: GenParser Char st ()
manySpaces = many1 spaceChar *> return ()

sepBySpaces :: GenParser Char st a ->  GenParser Char st [a]
sepBySpaces a = sepBy a manySpaces

sepBySpaces1 :: GenParser Char st a ->  GenParser Char st [a]
sepBySpaces1 a = sepBy1 a manySpaces

onePerLine :: GenParser Char st a -> GenParser Char st [a]
onePerLine orgparser =
    do
    fs <-  many $ orgparser <* endOfLine
    rest <- optionMaybe orgparser <&> maybeToList
    optional $ many endOfLine
    return $ fs ++ rest
