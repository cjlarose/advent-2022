{-# LANGUAGE OverloadedStrings #-}

module Advent2021.Day02
  ( solve
  ) where

import Numeric.Natural (Natural)
import Text.Megaparsec ((<|>), eof, some)
import Control.Monad.Combinators (choice)
import Data.Text (Text)
import Data.List (foldl')
import Control.Monad.State (State, evalState, get, put)
import Advent.Input (getProblemInputAsText)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair(..))
import Advent.Parse (Parser, parse)
import Advent.CommonParsers (linesOf, symbol, token, natural)

data Command = Forward Natural | Down Natural | Up Natural

inputParser :: Parser [Command]
inputParser = some command <* eof
  where
    commandP :: Text -> (Natural -> Command) -> Parser Command
    commandP literal f = symbol literal *> (f <$> token natural)

    command :: Parser Command
    command = choice [ commandP "forward" Forward
                     , commandP "down" Down
                     , commandP "up" Up
                     ]

data Submarine = Submarine { horizontalPosition :: Int, depth :: Int }
data SubmarineState = SubmarineState { submarine :: Submarine, aim :: Int }

totalDisplacement :: [Command] -> Submarine
totalDisplacement xs = evalState (runSub xs) (SubmarineState (Submarine 0 0) 0)
  where
    runSub :: [Command] -> State SubmarineState Submarine
    runSub [] = do
      SubmarineState sub _ <- get
      return sub
    runSub ((Forward x):xs) = do
      state@(SubmarineState sub _) <- get
      let newSub = sub{horizontalPosition=horizontalPosition sub + fromIntegral x}
      put state{submarine=newSub}
      runSub xs
    runSub ((Down x):xs) = do
      state@(SubmarineState sub _) <- get
      let newSub = sub{depth=depth sub + fromIntegral x}
      put state{submarine=newSub}
      runSub xs
    runSub ((Up x):xs) = do
      state@(SubmarineState sub _) <- get
      let newSub = sub{depth=depth sub - fromIntegral x}
      put state{submarine=newSub}
      runSub xs

printResults :: [Command] -> PuzzleAnswerPair
printResults depths = PuzzleAnswerPair (part1, part2)
  where
    Submarine pos depth = totalDisplacement depths
    part1 = show $ pos * depth
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 2
