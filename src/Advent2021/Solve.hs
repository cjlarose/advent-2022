module Advent2021.Solve
  ( solverForProblem
  ) where

import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import qualified Advent2021.Day01

solverForProblem :: Int -> IO (Either String PuzzleAnswerPair)
solverForProblem 1 = Advent2021.Day01.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
