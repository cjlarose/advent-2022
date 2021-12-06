module Advent2021.Solve
  ( solverForProblem
  ) where

import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import qualified Advent2021.Day01
import qualified Advent2021.Day02

solverForProblem :: Int -> IO (Either String PuzzleAnswerPair)
solverForProblem 1 = Advent2021.Day01.solve
solverForProblem 2 = Advent2021.Day02.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
