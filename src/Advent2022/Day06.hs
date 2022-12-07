module Advent2022.Day06
  ( solve,
  )
where

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Advent.CommonParsers (token, symbol, natural)
import Data.List (transpose)
import Control.Monad.State (State, evalState, get, put)
import Text.Megaparsec (some, sepBy1, (<|>), count, try, eof)
import Text.Megaparsec.Char (char, digitChar, lowerChar, newline, eol)

inputParser :: Parser String
inputParser = some lowerChar <* newline <* eof

prefixLength :: Int -> String -> Int
prefixLength dropped (a:b:c:d:xs)
  | a /= b && a /= c && a /= c && a /= d && b /= c && b /= d && c /= d = dropped + 4
  | otherwise = prefixLength (dropped + 1) $ b:c:d:xs

printResults :: String -> PuzzleAnswerPair
printResults input = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . prefixLength 0 $ input
    part2 = ""

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 6
