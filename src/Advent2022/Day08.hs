{-# LANGUAGE OverloadedStrings #-}
module Advent2022.Day08
  ( solve,
  )
where

import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Data.Char (digitToInt)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Megaparsec (some, eof)
import Text.Megaparsec.Char (newline, eol, digitChar)

type TreeGrid = Map.Map (Int, Int) Int

inputParser :: Parser TreeGrid
inputParser = toGrid Map.empty 0 <$> some (row <* newline) <* eof
  where
    row = some (digitToInt <$> digitChar)
    toGrid grid _ [] = grid
    toGrid grid i (x : xs) =
      let newGrid = foldl (\acc (j, height) -> Map.insert (i, j) height acc) grid . zip [0..] $ x
      in toGrid newGrid (i + 1) xs

treesInSameLine :: TreeGrid -> (Int, Int) -> (Int, Int) -> TreeGrid
treesInSameLine grid (di, dj) coord = Map.restrictKeys grid keySet
  where
    nextCoord (i, j) = (i + di, j + dj)
    keySet = Set.fromList . takeWhile (\x -> Map.member x grid) . drop 1 . iterate nextCoord $ coord

numVisibleTrees :: TreeGrid -> Int
numVisibleTrees grid = Map.size . Map.filterWithKey isVisible $ grid
  where
    isVisible coord h = or . map (isVisibleFromDir coord h) $ [(0, 1), (0, -1), (1, 0), (-1, 0)]
    isVisibleFromDir coord h dir = not . any (\x -> x >= h) . Map.elems . treesInSameLine grid dir $ coord

printResults :: TreeGrid -> PuzzleAnswerPair
printResults grid = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . numVisibleTrees $ grid
    part2 = ""

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 8
