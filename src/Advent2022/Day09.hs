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

uniqueTailPositions :: [Motion] -> Int
uniqueTailPositions motions = length . nub $ allTailPositions
  where
    move (U x) (i, j) = [(i - di, j) | di <- [1..x]]
    move (D x) (i, j) = [(i + di, j) | di <- [1..x]]
    move (L x) (i, j) = [(i, j - dj) | dj <- [1..x]]
    move (R x) (i, j) = [(i, j + dj) | dj <- [1..x]]
    manhattanDistance (a, b) (c, d) = abs (a - c) + abs (b - d)
    neighbors (i, j) = [(i + di, j + dj) | di <- [-1..1], dj <- [-1..1]]
    adjacent x y = elem y . neighbors $ x

    headPositions pos [] = [pos]
    headPositions pos (motion : xs) =
      let intermediatePos = move motion pos
      in pos : init intermediatePos ++ headPositions (last intermediatePos) xs

    allHeadPositions = headPositions (0, 0) motions

    tailPositions pos [] = [pos]
    tailPositions pos (headPos : xs) =
      let newTail = if adjacent headPos pos
                    then pos
                    else minimumBy (comparing $ manhattanDistance headPos) . neighbors $ pos
      in newTail : tailPositions newTail xs

    allTailPositions = tailPositions (0, 0) allHeadPositions

printResults :: [Motion] -> PuzzleAnswerPair
printResults motions = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . uniqueTailPositions $ motions
    part2 = "TODO"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 9
