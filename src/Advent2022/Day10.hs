{-# LANGUAGE OverloadedStrings #-}
module Advent2022.Day10
  ( solve,
  )
where

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Advent.CommonParsers (integerWithOptionalLeadingSign, symbol, token)
import Data.Char (digitToInt)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map ((!))
import Text.Megaparsec (some, eof, (<|>))
import Text.Megaparsec.Char (newline, eol, digitChar)

data Instruction = Noop | Addx Int

inputParser :: Parser [Instruction]
inputParser = some instruction <* eof
  where
   instruction = noop <|> addx
   noop = Noop <$ symbol "noop"
   addx = Addx <$> (symbol "addx" *> token integerWithOptionalLeadingSign)

machineStates :: [Instruction] -> [(Int, Int)]
machineStates = f (1, 1)
  where
    f (c, x) [] = []
    f (c, x) (Noop : xs) = (c, x) : f (c + 1, x) xs
    f (c, x) (Addx y : xs) = (c, x) : (c + 1, x) : f (c + 2, x + y) xs

signalStrengths :: [Int] -> [Instruction] -> [Int]
signalStrengths keyCycles = f keyCycles . machineStates
  where
    f [] states = []
    f (keyCycle : xs) states =
      let
        ((c, x) : states') = dropWhile (\(c, _) -> c < keyCycle) states
      in c * x : f xs states'

printResults :: [Instruction] -> PuzzleAnswerPair
printResults instructions = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sum . signalStrengths [20, 60..220] $ instructions
    part2 = "TODO"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 10
