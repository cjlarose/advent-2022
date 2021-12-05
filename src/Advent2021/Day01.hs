module Advent2021.Day01
  ( solve
  ) where

import Numeric.Natural (Natural)

import Advent.Input (getProblemInputAsText)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.Parse (Parser, parse)
import Advent.CommonParsers (linesOf, natural)
import Advent.ListUtils (consectutivePairs)

inputParser :: Parser [Natural]
inputParser = linesOf natural

numIncreases :: [Natural] -> Int
numIncreases = length . filter (\(a, b) -> b > a) . consectutivePairs

printResults :: [Natural] -> PuzzleAnswerPair
printResults depths = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . numIncreases $ depths
    part2 = ""

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 1
