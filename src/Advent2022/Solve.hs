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

solverForProblem :: Int -> IO (Either String PuzzleAnswerPair)
solverForProblem 1 = Advent2022.Day01.solve
solverForProblem 2 = Advent2022.Day02.solve
solverForProblem 3 = Advent2022.Day03.solve
solverForProblem 4 = Advent2022.Day04.solve
solverForProblem 5 = Advent2022.Day05.solve
solverForProblem n = pure . Left $ "Unknown problem " ++ show n
