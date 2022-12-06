module Advent2022.Solve
  ( solverForProblem,
  )
where

import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import qualified Advent2022.Day01

solverForProblem :: Int -> IO (Either String PuzzleAnswerPair)
solverForProblem 1 = Advent2022.Day01.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
