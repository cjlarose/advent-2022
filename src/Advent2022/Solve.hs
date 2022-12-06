module Advent2022.Solve
  ( solverForProblem,
  )
where

import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import qualified Advent2022.Day01
import qualified Advent2022.Day02

solverForProblem :: Int -> IO (Either String PuzzleAnswerPair)
solverForProblem 1 = Advent2022.Day01.solve
solverForProblem 2 = Advent2022.Day02.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
