module Advent2022.Day02
  ( solve,
  )
where

import Advent.CommonParsers (natural, token)
import Advent.Input (getProblemInputAsText)
import Advent.Parse (Parser, parse)
import Advent.PuzzleAnswerPair (PuzzleAnswerPair (..))
import Numeric.Natural (Natural)
import Data.List (sort)
import Text.Megaparsec (eof, some, sepBy1, (<|>))
import Text.Megaparsec.Char (newline, char)

data OpponentChoice = A | B | C
data PlayerResponse = X | Y | Z
type GameStrat = (OpponentChoice, PlayerResponse)

inputParser :: Parser [GameStrat]
inputParser = some game <* eof

game :: Parser GameStrat
game = (,) <$> token opponentChoice <*> token playerResponse

opponentChoice :: Parser OpponentChoice
opponentChoice = A <$ char 'A' <|> B <$ char 'B' <|> C <$ char 'C'

playerResponse :: Parser PlayerResponse
playerResponse = X <$ char 'X' <|> Y <$ char 'Y' <|> Z <$ char 'Z'

predictedScore :: [GameStrat] -> Int
predictedScore = sum . map gameScore
  where
    gameScore game = shapeScore game + outcomeScore game
    shapeScore (_, X) = 1
    shapeScore (_, Y) = 2
    shapeScore (_, Z) = 3
    outcomeScore (A, X) = 3
    outcomeScore (A, Y) = 6
    outcomeScore (A, Z) = 0
    outcomeScore (B, X) = 0
    outcomeScore (B, Y) = 3
    outcomeScore (B, Z) = 6
    outcomeScore (C, X) = 6
    outcomeScore (C, Y) = 0
    outcomeScore (C, Z) = 3

printResults :: [GameStrat] -> PuzzleAnswerPair
printResults games = PuzzleAnswerPair (part1, part2)
  where
    part1 = show . predictedScore $ games
    part2 = ""

solve :: IO (Either String PuzzleAnswerPair)
solve = parse inputParser printResults <$> getProblemInputAsText 2
