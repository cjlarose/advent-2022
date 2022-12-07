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

prefixLength :: [Int] -> Int
prefixLength xs = f 0 initialBitSet xs
  where
    initialBitSet :: Int
    initialBitSet = foldl' (\acc x -> setBit acc x) 0 . take 4 $ xs

    f :: Int -> Int -> [Int] -> Int
    f dropped bits xs
      | popCount (setBit bits (xs !! 3)) == 4 = dropped + 3
      | otherwise = f (dropped + 1) (setBit (clearBit bits (head xs)) (xs !! 3)) (tail xs)

printResults :: [Int] -> PuzzleAnswerPair
printResults input = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . prefixLength $ input
    part2 = ""

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 6
