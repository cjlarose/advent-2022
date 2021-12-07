module Advent2021.Day03
  ( solve,
  )
where

import Advent.BitUtils (fromBits)
import Advent.CommonParsers (linesOf, unsignedBinaryInteger)
import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Data.Bits (testBit, xor)
import Data.List (group, sort)

newtype DianosticEntry = DianosticEntry Int deriving (Show)

inputParser :: Parser [DianosticEntry]
inputParser = linesOf (DianosticEntry <$> unsignedBinaryInteger)

gamma :: [DianosticEntry] -> Int
gamma diagnostics = fromBits . map mostCommonBitInPosition $ [11, 10 .. 0]
  where
    bitsInPosition :: Int -> [Bool]
    bitsInPosition i = map (\(DianosticEntry x) -> testBit x i) diagnostics

    mostCommonBitInPosition :: Int -> Bool
    mostCommonBitInPosition i = if numZeros > numOnes then False else True
      where
        numZeros : numOnes : _ = map length . group . sort . bitsInPosition $ i

powerConsumption :: [DianosticEntry] -> Int
powerConsumption diagnostics = gammaRate * epsilonRate
  where
    gammaRate = gamma diagnostics
    epsilonRate = gammaRate `xor` 0xFFF

printResults :: [DianosticEntry] -> PuzzleAnswerPair
printResults diagnostics = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . powerConsumption $ diagnostics
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 3
