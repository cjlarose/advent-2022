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
import Data.Map ((!))
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

treesInSameLine :: TreeGrid -> (Int, Int) -> (Int, Int) -> [((Int, Int), Int)]
treesInSameLine grid (di, dj) coord = map (\k -> (k, grid ! k)) $ keys
  where
    nextCoord (i, j) = (i + di, j + dj)
    keys = takeWhile (\x -> Map.member x grid) . drop 1 . iterate nextCoord $ coord

allDirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]

numVisibleTrees :: TreeGrid -> Int
numVisibleTrees grid = Map.size . Map.filterWithKey isVisible $ grid
  where
    isVisible coord h = or . map (isVisibleFromDir coord h) $ allDirs
    isVisibleFromDir coord h dir = not . any (\x -> x >= h) . map snd . treesInSameLine grid dir $ coord

highestScenicScore :: TreeGrid -> Int
highestScenicScore grid = maximum . map scenicScore . Map.assocs $ grid
  where
    scenicScore (coord, height) = product . map (visibleTreesInDir coord height) $ allDirs
    visibleTreesInDir coord height dir = f 0 . treesInSameLine grid dir $ coord
      where
        f n [] = n
        f n ((c, h) : xs) | h >= height = n + 1
                          | otherwise = f (n + 1) xs

printResults :: TreeGrid -> PuzzleAnswerPair
printResults grid = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . numVisibleTrees $ grid
    part2 = show . highestScenicScore $ grid

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 8
