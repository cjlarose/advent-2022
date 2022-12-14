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
import Text.Megaparsec (some, eof, (<|>))
import Text.Megaparsec.Char (newline, eol)

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

grid :: [Instruction] -> String
grid = unlines . rows . pixels . machineStates
  where
    rows [] = []
    rows xs = let (row, rest) = splitAt 40 xs in row : rows rest
    pixels [] = []
    pixels ((c, x) : xs) =
      let
        y = (c - 1) `mod` 40
        lit = abs (y - x) <= 1
        pixel = if lit then '#' else '.'
      in pixel : pixels xs

printResults :: [Instruction] -> PuzzleAnswerPair
printResults instructions = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . sum . signalStrengths [20, 60..220] $ instructions
    part2 = grid $ instructions

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 10
