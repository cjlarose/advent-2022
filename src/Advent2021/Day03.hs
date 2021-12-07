module Advent2021.Day03
  ( solve
  ) where

import Advent.Input (getProblemInputAsText)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.Parse (Parser, parse)
import Advent.CommonParsers (linesOf, unsignedBinaryInteger)

newtype DianosticEntry = DianosticEntry Int deriving Show

inputParser :: Parser [DianosticEntry]
inputParser = linesOf entry
  where
    entry = DianosticEntry <$> unsignedBinaryInteger

printResults :: [DianosticEntry] -> PuzzleAnswerPair
printResults diagnostics = PuzzleAnswerPair (part1, part2)
  where
    part1 = "not yet implemented"
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 3
