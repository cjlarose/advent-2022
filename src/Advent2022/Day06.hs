module Advent2022.Day06
  ( solve,
  )
where

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char (lowerChar, newline, eol)

inputParser :: Parser String
inputParser = some lowerChar <* newline <* eof

prefixLength :: String -> Int
prefixLength = f 0
  where
    f :: Int -> String -> Int
    f dropped (a:b:c:d:xs)
      | a /= b && a /= c && a /= c && a /= d && b /= c && b /= d && c /= d = dropped + 4
      | otherwise = f (dropped + 1) $ b:c:d:xs

printResults :: String -> PuzzleAnswerPair
printResults input = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . prefixLength $ input
    part2 = ""

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 6
