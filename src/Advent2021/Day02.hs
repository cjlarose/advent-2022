{-# LANGUAGE OverloadedStrings #-}

module Advent2021.Day02
  ( solve
  ) where

import Numeric.Natural (Natural)
import Text.Megaparsec ((<|>), eof, some)
import Control.Monad.Combinators (choice)
import Data.Text (Text)
import Data.List (foldl')
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

totalDisplacement :: [Command] -> (Int, Int)
totalDisplacement = foldl' f (0, 0)
  where
    f (pos, depth) (Forward x) = (pos + fromIntegral x, depth)
    f (pos, depth) (Down x) = (pos, depth + fromIntegral x)
    f (pos, depth) (Up x) = (pos, depth - fromIntegral x)

printResults :: [Command] -> PuzzleAnswerPair
printResults depths = PuzzleAnswerPair (part1, part2)
  where
    (pos, depth) = totalDisplacement depths
    part1 = show $ pos * depth
    part2 = "not yet implemented"

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 2
