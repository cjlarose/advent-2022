module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Advent2022.Solve (solverForProblem)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> do
      let n = read arg
      result <- solverForProblem n
      case result of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right (PuzzleAnswerPair (part1, part2)) -> do
          let output = part1 ++ "\n" ++ part2
          putStrLn output
    _ -> hPutStrLn stderr "Usage: advent2022-exe problem-number"
