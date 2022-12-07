module Advent2022.Day06
  ( solve,
  )
where

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Data.Char (ord)
import Data.List (foldl')
import Data.Bits (setBit, clearBit, popCount)
import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char (lowerChar, newline, eol)

inputParser :: Parser [Int]
inputParser = map (\c -> ord c - ord 'a') <$> some lowerChar <* newline <* eof

prefixLength :: Int -> [Int] -> Int
prefixLength n xs = f 0 xs
  where
    toBitSet :: [Int] -> Int
    toBitSet = foldl' (\acc x -> setBit acc x) 0

    f :: Int -> [Int] -> Int
    f dropped xs
      | popCount (toBitSet . take n $ xs) == n = dropped + n
      | otherwise = f (dropped + 1) (tail xs)

printResults :: [Int] -> PuzzleAnswerPair
printResults input = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . prefixLength 4 $ input
    part2 = show . prefixLength 14 $ input

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 6
