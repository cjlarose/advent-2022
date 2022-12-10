{-# LANGUAGE OverloadedStrings #-}
module Advent2022.Day09
  ( solve,
  )
where

import Data.List (nub, minimumBy)
import Data.Ord (comparing)
import Text.Megaparsec (some, eof, (<|>))
import Text.Megaparsec.Char (char, newline, eol, digitChar)
import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Advent.CommonParsers (integerWithOptionalLeadingSign, token)

data Motion = U Int | D Int | L Int | R Int

inputParser :: Parser [Motion]
inputParser = some (motion <* newline) <* eof
  where
    motion = foldl1 (<|>) . map (\(f, c) -> f <$> (token (char c) *> integerWithOptionalLeadingSign)) $ [(U, 'U'), (D, 'D'), (L, 'L'), (R, 'R')]

headPositions :: [Motion] -> [(Int, Int)]
headPositions = f (0, 0)
  where
    move (U x) (i, j) = [(i - di, j) | di <- [1..x]]
    move (D x) (i, j) = [(i + di, j) | di <- [1..x]]
    move (L x) (i, j) = [(i, j - dj) | dj <- [1..x]]
    move (R x) (i, j) = [(i, j + dj) | dj <- [1..x]]
    f pos [] = [pos]
    f pos (motion : xs) =
      let intermediatePos = move motion pos
      in pos : init intermediatePos ++ f (last intermediatePos) xs

tailPositions :: [(Int, Int)] -> [(Int, Int)]
tailPositions = f (0, 0)
  where
    manhattanDistance (a, b) (c, d) = abs (a - c) + abs (b - d)
    neighbors (i, j) = [(i + di, j + dj) | di <- [-1..1], dj <- [-1..1]]
    adjacent (xi, xj) (yi, yj) = abs (xi - yi) <= 1 && abs (xj - yj) <= 1

    f pos [] = [pos]
    f pos (headPos : xs) =
      let newTail = if adjacent headPos pos
                    then pos
                    else minimumBy (comparing $ manhattanDistance headPos) . neighbors $ pos
      in newTail : f newTail xs

uniqueTailPositions :: [Motion] -> Int
uniqueTailPositions motions = length . nub . tailPositions . headPositions $ motions

ninthTailPositions :: [Motion] -> Int
ninthTailPositions motions = length . nub . (\x -> x !! 9) . iterate tailPositions . headPositions $ motions

printResults :: [Motion] -> PuzzleAnswerPair
printResults motions = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . uniqueTailPositions $ motions
    part2 = show . ninthTailPositions $ motions

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 9
