module Advent2022.Solve
  ( solverForProblem,
  )
where

import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import qualified Advent2022.Day01
import qualified Advent2022.Day02
import qualified Advent2022.Day03
import qualified Advent2022.Day04
import qualified Advent2022.Day05
import qualified Advent2022.Day06
import qualified Advent2022.Day07
import qualified Advent2022.Day08
import qualified Advent2022.Day09

solverForProblem :: Int -> IO (Either String PuzzleAnswerPair)
solverForProblem 1 = Advent2022.Day01.solve
solverForProblem 2 = Advent2022.Day02.solve
solverForProblem 3 = Advent2022.Day03.solve
solverForProblem 4 = Advent2022.Day04.solve
solverForProblem 5 = Advent2022.Day05.solve
solverForProblem 6 = Advent2022.Day06.solve
solverForProblem 7 = Advent2022.Day07.solve
solverForProblem 8 = Advent2022.Day08.solve
solverForProblem 9 = Advent2022.Day09.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
