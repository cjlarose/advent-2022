module Advent2022.Day01
  ( solve,
  )
where

import Advent.CommonParsers (natural)
import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Numeric.Natural (Natural)
import Data.List (sort)
import Text.Megaparsec (eof, some, sepBy1)
import Text.Megaparsec.Char (newline)

inputParser :: Parser [[Natural]]
inputParser = (elfInventory `sepBy1` newline) <* eof

elfInventory :: Parser [Natural]
elfInventory = some (natural <* newline)

greatestCalories :: [[Natural]] -> Natural
greatestCalories = maximum . map sum

caloriesOfTopThreeElves :: [[Natural]] -> Natural
caloriesOfTopThreeElves = sum . take 3 . reverse . sort . map sum

printResults :: [[Natural]] -> PuzzleAnswerPair
printResults elfInventories = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . greatestCalories $ elfInventories
    part2 = show . caloriesOfTopThreeElves $ elfInventories

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 1
