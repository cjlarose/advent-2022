module Advent2021.Solve
  ( solverForProblem
  ) where

import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import qualified Advent2021.Day01
import qualified Advent2021.Day02
import qualified Advent2021.Day03

solverForProblem :: Int -> IO (Either String PuzzleAnswerPair)
solverForProblem 1 = Advent2021.Day01.solve
solverForProblem 2 = Advent2021.Day02.solve
solverForProblem 3 = Advent2021.Day03.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
