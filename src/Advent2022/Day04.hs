module Advent2022.Day04
  ( solve,
  )
where

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Advent.CommonParsers (linesOf)
import Data.List (sort, intersect)
import Data.Char (isLower, ord)
import Text.Megaparsec (some)
import Text.Megaparsec.Char (char, digitChar)

type SectionAssignment = (Int, Int)
type AssignmentPair = (SectionAssignment, SectionAssignment)

inputParser :: Parser [AssignmentPair]
inputParser = linesOf assignmentPair
  where
    assignmentPair = (,) <$> sectionAssignment <* char ',' <*> sectionAssignment
    sectionAssignment = (,) <$> section <* char '-' <*> section
    section = read <$> some digitChar

numSubsets :: [AssignmentPair] -> Int
numSubsets = length . filter hasSubset
  where
    hasSubset ((a, b), (x, y)) = x >= a && y <= b || a >= x && b <= y

numOverlapping :: [AssignmentPair] -> Int
numOverlapping = length . filter hasOverlap
  where
    hasOverlap ((a, b), (x, y)) = x >= a && x <= b || y >= a && y <= b || a >= x && a <= y || b >= x && b <= y

printResults :: [AssignmentPair] -> PuzzleAnswerPair
printResults sacks = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . numSubsets $ sacks
    part2 = show . numOverlapping $ sacks

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 4
